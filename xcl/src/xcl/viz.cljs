(ns xcl.viz
  (:require [reagent.core :as r]
            [goog.dom :as gdom]
            [xcl.core :as sc]
            [xcl.corpus :as corpus]
            [xcl.common :refer [get-file-extension]]
            [xcl.content-interop :as ci]
            [xcl.external-js-content :as ext-js]
            [xcl.external :as ext]))

(defn get-static-content
  [search-path]
  (corpus/file-cache search-path))

(defn render-map [m]
  {:pre [(map? m)]}
  (->> m
       (sort)
       (map (fn [[k v]]
              [:tr
               [:th [:code k]]
               [:td [:code (subs
                            (pr-str v)
                            0 500)]]]))
       (concat [:tbody])
       (vec)
       (vector :table {:style {:font-size "x-small"}})
       (vec)))

;; TODO: revisit the pdb/epub special case ugliness
(defn resolve-resource-spec-async
  [link on-resolved-resource-spec]
  (sc/get-resource-match-async
   ;; candidate-seq-loader-async
   (fn [resource-name-matcher
        callback]

     (if (#{"pdf" "epub"}
          (get-file-extension resource-name-matcher))
       (do
         (callback [resource-name-matcher]))
       (->> (corpus/list-files
             resource-name-matcher)
            (callback))))
   
   ;; content-loader-async
   (fn [resolved-spec callback]
     (js/console.log "hitting content loader for"
                     (pr-str
                      (select-keys resolved-spec
                                   [:resource-resolver-method
                                    :resource-resolver-path])))
     (let [path (:resource-resolver-path resolved-spec)]
       (when-let [content (corpus/file-cache path)]
         (callback content))))
   
   ;; link
   link
   
   ;; callback
   on-resolved-resource-spec))

(defn load-content-async
  [resource-spec on-content]
  (let [path (:resource-resolver-path resource-spec)
        extension (get-file-extension path)]
    (js/console.warn
     (str
      "%c ===> resource address %c "
      path)
     "color:white;background:red;"
     "color:black;background:yellow;")
   
    (if-let [external-loader (@ext/$ExternalLoaders extension)]
      (external-loader
       resource-spec on-content)
      (js/setTimeout
       (fn []
         (on-content
          (corpus/file-cache path)))
       (* 1000 (Math/random))))))

(defn render-highlights-in-text [text highlights]
  (loop [remain (reverse highlights)
         current-index (count text)
         out []]
    (if (empty? remain)
      (reverse
       (if (= 0 current-index)
             out
             (conj out [:span (subs text 0 current-index)])))
      (let [highlight (first remain)
            h-index (:index highlight)
            h-length (:length highlight)
            h-end (+ h-index h-length)]
        (recur (rest remain)
               h-index
               (conj (if (< h-end current-index)
                       (conj out [:span (subs text h-end current-index)])
                       out)
                     [:span
                      {:style {:border "1px solid red"}}
                      (subs text h-index h-end)]))))))

(defn render-text-anchoring-test-view! [view-state]
  [:div

   [:table
    {:style {:border-collapse "collapse"}}
    [:style "td { border: 1px solid gray; }"]
    [:tbody
     [:tr
      [:th "content"]
      [:th "target"]
      [:th "matches"]]
     (->> [[" a b c d f g "
            " b d       f"]
           [" one two three four five  "
            "  four six "]]
          (map (fn [[content target]]
                 [:tr
                  [:td content]
                  [:td target]
                  [:td
                   [:pre
                    (-> (sc/find-successive-tokens-in-content
                         content
                         (-> target
                             (clojure.string/trim)
                             (clojure.string/split #"\s+")))
                        (pr-str))]]])))]]
   
   (let [doc-names ["tiny.org"
                    "big.org"
                    "fake.org"
                    "dummy.org"
                    "xcl-test-3-a.org"]
         doc-blobs (map get-static-content doc-names)
         targets
         ["  fourth line "
          " 5th   line "
          "  fake      file  "
          "ake"
          "  you "
          "you"
          "  in    the  "
          " in and CATS"
          " aye aye"]

         all-matches (sc/find-corpus-matches-by-tokenization
                      doc-blobs targets)]
     [:table
      {:style {:border-collapse "collapse"}}
      [:style "td { border: 1px solid gray; }"]
      [:tbody
       [:tr
        [:th "index"]
        [:th "corpus source"]
        [:th "hit strings"]
        [:th "(rendered) content"]
        [:th "hits"]]
       (->> (map vector (range (count doc-names)) doc-names doc-blobs)
            (map (fn [[index fname content]]
                   (let [matches-for-index
                         (when-let [maybe-matches (get all-matches index)]
                           maybe-matches)]
                     [:tr
                      [:td index]
                      [:td fname]
                      [:td
                       [:ul
                        (->> matches-for-index
                             (map :target)
                             (map (partial vector :li)))]
                       ]
                      [:td
                       [:pre
                        (render-highlights-in-text
                         content
                         (->> matches-for-index
                              (map :matches)
                              (apply concat)))]]
                      [:td
                       [:pre
                        {:style {:font-size "xx-small"}}
                        (-> (clj->js matches-for-index)
                            (js/JSON.stringify nil 2))]]]))))]])])

(defn render-resource-resolver-test-view! [view-state]
  (let [cases [["exact match"
                "LICENSE" "LICENSE"]
               ["exact match"
                "file:tiny.org" "tiny.org"]
               ["glob file name"
                "file:d*y.org" "dummy.org"]
               ["fuzzy find file by content +"
                "grep:ZZ+you" "dummy.org"]]]
    (->> cases
         (map (fn [[desc link expected-name]]
                (let [received-name (r/atom nil)]
                  [(fn []
                     
                     (resolve-resource-spec-async
                      link
                      (fn [resolved-resource-spec]
                        (reset! received-name
                                (:resource-resolver-path
                                 resolved-resource-spec))))
                     
                     (let [success? (if (nil? @received-name)
                                      nil
                                      (= expected-name @received-name))]
                       (if-not (and (get-in @view-state [:hide-passing?])
                                    success?)
                         [:tr
                          [:td
                           {:style (case success?
                                     true {:background-color "lime" :color "white"}
                                     false {:background-color "red" :color "white"}
                                     {})}
                           (case success?
                             true "PASS"
                             false "FAIL"
                             "")]
                          [:td desc]
                          [:td link]
                          [:td expected-name]
                          [:td @received-name]])))])))
         (concat [:tbody
                  [:tr
                   [:th "PASS?"]
                   [:th "description"]
                   [:th "link expression"]
                   [:th "expected"]
                   [:th "received"]]])
         (vec)
         (vector :table
                 {:style {:border-collapse "collapse"}}
                 [:style "td { border: 1px solid gray; }"])
         (vec)
         (vector :div
                 [:h2 "resource resolver test"]))))

(defn render-link-test-view! [view-state]
  (->> [["line in file"
         "LICENSE::7"
         "of this license document, but changing it is not allowed."
         "LICENSE" :exact-name
         :line-range {:beg 7 :end 7}]
        ["line in file"
         "file:100lines::5"
         "5 SOME LINE"
         "100lines" :exact-name
         :line-range {:beg 5 :end 5}]
        ["line range"
         "file:tiny.org::2-3"
         "* decoy 1\n* something third line"
         ;; (get-static-content "tiny.org")
         "tiny.org" :exact-name
         :line-range {:beg 2 :end 3}]
        ["line from start"
         "file:tiny.org::-2"
         "fake file (line 1)\n* decoy 1"
         ;; (get-static-content "tiny.org")
         "tiny.org" :exact-name
         :line-range {:beg nil :end 2}]
        ["line to end"
         "file:tiny.org::7-"
         "seven 7s\nocho acht"
         "tiny.org" :exact-name
         :line-range {:beg 7 :end nil}]
        ["character range"
         "tiny.org::5,40"
         "file (line 1)\n* decoy 1\n* something"
         "tiny.org" :exact-name
         :char-range {:beg 5 :end 40}]
        ["character from start"
         "file:tiny.org::,20"
         "fake file (line 1)\n*"
         "tiny.org" :exact-name
         :char-range {:beg nil :end 20}]
        ["character to end"
         "file:tiny.org::75,"
         "h line\nsix sixths is sick sith\nseven 7s\nocho acht"
         "tiny.org" :exact-name
         :char-range {:beg 75 :end nil}]
        ["percent range"
         "100lines::1%-3%"
         "2 SOME LINE\n3 SOME LINE\n4 SOME LINE"
         "100lines" :exact-name
         :percent-range {:beg 1 :end 3}]
        ["native org: heading"
         "file:tiny.org::*decoy 1"
         "* decoy 1"
         "tiny.org" :exact-name
         :org-heading {:heading "decoy 1"}]
        ["exact string match range"
         "file:dummy.org::in 2101...for great justice"
         "In 2101, war was beginning. What happen? Main screen turn on. For great justice"
         "dummy.org" :exact-name
         :token-bound {:token-beg "in 2101"
                       :token-end "for great justice"}]
        ["glob file name"
         "file:d*y.org::*huh"
         "* huh\n\nwhatever is in the block"
         "d*y.org" :glob-name
         :org-heading {:heading "huh"}]
        ["fuzzy find file by content +"
         "grep:ZZ+you::*huh"
         "* huh\n\nwhatever is in the block"
         "ZZ+you" :grep-content
         :org-heading {:heading "huh"}]
        ["fuzzy find file by content raw space"
         "grep:ZZ you::*huh"
         "* huh\n\nwhatever is in the block"
         "ZZ you" :grep-content
         :org-heading {:heading "huh"}]
        ["fuzzy find file by html entity"
         "grep:ZZ%20you::*huh"
         "* huh\n\nwhatever is in the block"
         "ZZ you" :grep-content
         :org-heading {:heading "huh"}]
        ["constrict by org node ID"
         "xcl:dummy.org?id=my-node-id"
         "* next heading\n  :PROPERTIES:\n  :CUSTOM_ID: my-node-id\n  :END:\n\n  good stuff"
         "dummy.org" :exact-name
         :org-node-id {:id "my-node-id"}]
        ["constrict by first token range"
         "xcl:dummy.org?s=in 2101...for great justice."
         "In 2101, war was beginning. What happen? Main screen turn on. For great justice."
         "dummy.org" :exact-name
         :token-bound {:token-beg "in 2101"
                       :token-end "for great justice."}]
        ["constrict by nearest line"
         "xcl:dummy.org?line=support+scheduled"
         "Support attributes like ~SCHEDULED:~."
         "dummy.org" :exact-name
         :line-with-match {:query-string "support+scheduled"}]
        ["constrict by first matching paragraph"
         "xcl:dummy.org?para=what+happen"
         "In 2101, war was beginning. What happen? Main screen turn on. For great justice. Move ZIG."
         "dummy.org" :exact-name
         :paragraph-with-match
         {:query-string "what+happen"}]
        ["constrict by first matching section"
         "xcl:dummy.org?section=famous+script"
         "** famous script\n\n   Captain and CATS\n   \n   In 2101, war was beginning. What happen? Main screen turn on. For great justice. Move ZIG."
         "dummy.org" :exact-name
         :org-section-with-match {:query-string "famous+script"}]
        ["grab text from epub"
         "xcl:alice.epub?p=2-&s=Would you tell me, please...walk long enough"
         (->> ["Would you tell me, please, which way I ought to go from here?’"
               "‘That depends a good deal on where you want to get to,’ said the Cat."
               "‘I don’t much care where—’ said Alice."
               "‘Then it doesn’t matter which way you go,’ said the Cat."
               (str  "‘—so long as I get\n"
                     "somewhere\n"
                     ",’ Alice added as an explanation.")
               "‘Oh, you’re sure to do that,’ said the Cat, ‘if you only walk long enough"]
              (interpose "\n\n")
              (apply str))
         "alice.epub" :exact-name
         nil
         [{:type :page-number
           :bound {:beg 2 :end nil}}
          {:type :token-bound
           :bound {:token-beg "Would you tell me, please"
                   :token-end "walk long enough"}}]]
        ["grab text from pdf"
         "xcl:tracemonkey.pdf?p=3&s=Monkey observes that...so TraceMonkey attempts"
         "Monkey observes that it has reached an inner loop header that al- ready has a compiled trace, so TraceMonkey attempts"
         "tracemonkey.pdf" :exact-name
         nil
         [{:type :page-number
           :bound {:beg 3 :end 3}}
          {:type :token-bound
           :bound {:token-beg "Monkey observes that"
                   :token-end "so TraceMonkey attempts"}}]]
        ]
       (map (fn [[desc
                  link
                  expected-match-text
                  -resolver-path
                  -resolver-method
                  -maybe-main-resolver-type
                  -maybe-main-resolver-bound]]
              (let [expected-spec
                    (assoc
                     {:resource-resolver-path -resolver-path
                      :resource-resolver-method -resolver-method}
                     :content-resolvers
                     (if -maybe-main-resolver-type
                       [{:type -maybe-main-resolver-type
                         :bound -maybe-main-resolver-bound}]
                       -maybe-main-resolver-bound))
                    received-match-text (r/atom nil)]
                
                (resolve-resource-spec-async
                 link
                 (fn on-resolved [resolved-resource-spec]
                   (load-content-async
                    resolved-resource-spec
                    (fn [full-content]
                      (let [resolved-content
                            (some-> (ci/resolve-content
                                     resolved-resource-spec
                                     full-content)
                                    (clojure.string/trim))]
                        (reset! received-match-text
                                resolved-content))))))
                
                [(fn []
                   (let [received-spec (sc/parse-link link)
                         is-link-match?
                         (->> (keys expected-spec)
                              (map (fn [k]
                                     (= (k expected-spec)
                                        (k received-spec))))
                              (every? identity))

                         is-text-match?
                         (= expected-match-text
                            @received-match-text)
                         
                         success? (and is-link-match?
                                       is-text-match?)]
                     
                     (if (and success?
                              (get-in @view-state [:hide-passing?]))
                       nil
                       [:tr
                        {:style {:font-size "x-small"}}
                        [:td
                         {:style {:background-color
                                  (case success?
                                    true "lime"
                                    false "red"
                                    "")
                                  :color "white"}}
                         (case success?
                           true "PASS"
                           false "FAIL"
                           "")]
                        [:td desc]
                        [:td [:code link]]
                        [:td (render-map expected-spec)]
                        [:td
                         {:style {:background-color
                                  (case is-link-match?
                                    true "#CFC"
                                    false "#FCC"
                                    "")}}
                         (render-map received-spec)]
                        [:td
                         [:textarea
                          {:style {:font-size "x-small"
                                   :width "100%"
                                   :height "100%"}
                           :value expected-match-text}]]
                        [:td
                         {:style {:background-color
                                  (if @received-match-text
                                    (if (= expected-match-text
                                           @received-match-text)
                                      "#CFC"
                                      "#FCC")
                                    "#CCC")}}
                         [:textarea
                          {:style {:font-size "x-small"
                                   :width "100%"
                                   :height "100%"}
                           :value @received-match-text}]]])))])))
       (concat [:tbody
                [:tr
                 [:th "PASS?"]
                 [:th "description"]
                 [:th "link expression"]
                 [:th "expected resolve"]
                 [:th "resolves to"]
                 [:th "expected content"]
                 [:th "matched content"]]])
       (vec)
       (vector :table
               {:style {:border-collapse "collapse"}}
               [:style "td { border: 1px solid gray; }"])
       (vec)
       (vector :div
               [:h2 "link test view"])))

(defn render-transclusion-test-view! [view-state]
  (let [textarea (fn [content]
                   [:textarea
                    {:style {:width "20em"
                             :height "20em"}
                     :value content}])]
    (->> [["xcl-test-self-recursion.org"
           "I include myself:\nI include myself:\n{{{transclude(xcl:xcl-test-self-recursion.org)}}}"
           nil]
          ["xcl-test-infinite-1.org"
           "Hi from 1. I include infinite 2:\nHi from 2. I include infinite 1:\nHi from 1. I include infinite 2:\n{{{transclude(xcl:xcl-test-infinite-2.org)}}}"
           nil]
          ["xcl-test-1.org"
           "* blah blah\n\ngeneric content\ngenetic content\nanother fake file\n\n* huh\n\nwhatever is in the block"
           nil]
          ["xcl-test-2.org"
           "* fake file 2\n\nrandom block\ntandem block\n-----\n5 SOME LINE\n6 SOME LINE\n7 SOME LINE\n-----\n\n\n-----\nIn 2101, war was beginning. What happen? Main screen turn on. For great justice. Move ZIG.\n-----\n"
           [(fn [s _]
              (str "-----\n"
                   s "\n"
                   "-----\n"))]]
          ["xcl-test-3-c.org"
           "* I am C and I include B

*@1-!!* I am B and I include A

** @2-!!content of A!

aye aye aye??-2@??-1@"
           [(fn [s _]
              (str "!!" s "??"))
            (fn [s _ depth]
              (str "@" depth "-" s "-" depth "@"))]]
          ["xcl-test-3-d.org"
           "* I am D and I include A

#+BEGIN_TRANSCLUSION xcl-test-3-a.org :lines 1
@1 -- content of A!
#+END_TRANSCLUSION
"
           ;; custom transclusion directive postprocessor
           [(fn [s xcl-spec depth]
              (str "#+BEGIN_TRANSCLUSION "
                   (:resource-resolver-path xcl-spec)
                   " :lines "
                   (get-in xcl-spec
                           [:content-resolvers 0 :bound :beg])
                   "\n"
                   "@" depth " -- " s "\n"
                   "#+END_TRANSCLUSION\n"))]]]
         (map (fn [[source-file expected postprocessor-coll]]
                (let [source-text (get-static-content source-file)
                      rendered (apply
                                sc/render-transclusion
                                (fn [& _]
                                  (corpus/list-files "_test"))
                                get-static-content
                                source-text
                                postprocessor-coll)
                      is-same? (= expected rendered)]
                  [(fn []
                     (if (and (get-in @view-state [:hide-passing?])
                              is-same?)
                       nil
                       [:tr
                        [:td
                         {:style {:color "white"
                                  :background (if is-same?
                                                "lime"
                                                "red")}}
                         [:b source-file]]
                        [:td (textarea source-text)]
                        [:td (textarea expected)]
                        [:td (textarea rendered)]]))])))
         (concat [:tbody
                  [:tr
                   (->> ["source file"
                         "source text"
                         "expected"
                         "rendered"]
                        (map (fn [hdr]
                               [:th hdr])))]])
         (vec)
         (vector :table)
         (vec)
         (vector :div
                 [:h2 "transclusion test"]))))

(defn main []
  (let [view-state (r/atom {:hide-passing? false})]
    (r/render
     [:div
      [:div
       [:label
        [:input
         {:type "checkbox"
          :on-change #(swap! view-state update :hide-passing? not)}]
        "hide passing?"]]
      [:div (render-resource-resolver-test-view! view-state)]
      [:div (render-link-test-view! view-state)]
      [:div (render-transclusion-test-view! view-state)]
      [:div (render-text-anchoring-test-view! view-state)]
      [:div {:style {:clear "both"}}]]
     (gdom/getElement "main-app"))))

(main)

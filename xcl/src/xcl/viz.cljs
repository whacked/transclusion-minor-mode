(ns xcl.viz
  (:require [reagent.core :as r]
            [goog.dom :as gdom]
            [xcl.core :as sc]
            [xcl.corpus :as corpus]))

(defn get-static-content
  [search-path]
  (corpus/file-cache search-path))

(defn render-map [m]
  {:pre [(map? m)]}
  (->> m
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

(defn render-link-test-view! []
  ;; resolver record
  (defrecord RR
      [resource-resolver-method
       content-resolver-method-type
       resource-address
       match-content])

  (->> [

        ;; ["grab text from epub"
        ;;  "xcl:alice.epub?p=20&s=and...the"
        ;;  (RR. :exact-name-with-subsection
        ;;       :token-bound
        ;;       {:file-name "alice.epub"
        ;;        :page-number 20}
        ;;       "fail fail fail")]
        
        ;; ["grab text from pdf"
        ;;  "xcl:tracemonkey.pdf?p=3&s=Monkey observes that...so TraceMonkey attempts"
        ;;  (RR. :exact-name-with-subsection
        ;;       :token-bound
        ;;       {:file-name "tracemonkey.pdf"
        ;;        :page-number 3}
        ;;       "fail fail fail")]
        
        
        ["full file" "tiny.org"
         (RR. :exact-name
              :whole-file
              {:file-name "tiny.org"}
              (get-static-content "tiny.org"))]
        
        ["full file" "file:tiny.org"
         (RR. :exact-name
              :whole-file
              {:file-name "tiny.org"}
              (get-static-content "tiny.org"))]

        ["line in file" "LICENSE::7"
         (RR. :exact-name
              :line-range
              {:file-name "LICENSE"}
              "of this license document, but changing it is not allowed.")]
        ["line in file" "file:100lines::5"
         (RR. :exact-name
              :line-range
              {:file-name "100lines"}
              "5 SOME LINE")]
        ["line range" "file:tiny.org::2-3"
         (RR. :exact-name
              :line-range
              {:file-name "tiny.org"}
              "* decoy 1\n* something third line")]
        ["line from start" "file:tiny.org::-2"
         (RR. :exact-name
              :line-range
              {:file-name "tiny.org"}
              "fake file (line 1)\n* decoy 1")]
        ["line to end" "file:tiny.org::7-"
         (RR. :exact-name
              :line-range
              {:file-name "tiny.org"}
              "seven 7s\nocho acht")]
        ["character range" "tiny.org::5,40"
         (RR. :exact-name
              :char-range
              {:file-name "tiny.org"}
              "file (line 1)\n* decoy 1\n* something")]
        ["character from start" "file:tiny.org::,20"
         (RR. :exact-name
              :char-range
              {:file-name "tiny.org"}
              "fake file (line 1)\n*")]
        ["character to end" "file:tiny.org::75,"
         (RR. :exact-name
              :char-range
              {:file-name "tiny.org"}
              "h line\nsix sixths is sick sith\nseven 7s\nocho acht")]
        ["percent range" "100lines::1%-3%"
         (RR. :exact-name
              :percent-range
              {:file-name "100lines"}
              "2 SOME LINE\n3 SOME LINE\n4 SOME LINE")]
        ["native org: heading" "file:tiny.org::*decoy 1"
         (RR. :exact-name
              :org-heading
              {:file-name "tiny.org"}
              "* decoy 1")]
        ["exact string match range" "file:dummy.org::in 2101...for great justice"
         (RR. :exact-name
              :token-bound
              {:file-name "dummy.org"}
              "In 2101, war was beginning. What happen? Main screen turn on. For great justice")]
        ["glob file name" "file:d*y.org::*huh"
         (RR. :glob-name
              :org-heading
              {:file-name "dummy.org"}
              "* huh\n\nwhatever is in the block")]
        ["fuzzy find file by content +" "grep:ZZ+you::*huh"
         (RR. :grep-content
              :org-heading
              {:file-name "dummy.org"}
              "* huh\n\nwhatever is in the block")]
        ["fuzzy find file by content raw space" "grep:ZZ you::*huh"
         (RR. :grep-content
              :org-heading
              {:file-name "dummy.org"}
              "* huh\n\nwhatever is in the block")]
        ["fuzzy find file by html entity" "grep:ZZ%20you::*huh"
         (RR. :grep-content
              :org-heading
              {:file-name "dummy.org"}
              "* huh\n\nwhatever is in the block")]
        ["constrict by org node ID" "xcl:dummy.org?id=my-node-id"
         (RR. :exact-name
              :org-node-id
              {:file-name "dummy.org"}
              "* next heading\n  :PROPERTIES:\n  :CUSTOM_ID: my-node-id\n  :END:\n\n  good stuff")]
        ["constrict by first token range" "xcl:dummy.org?s=in 2101...for great justice."
         (RR. :exact-name
              :token-bound
              {:file-name "dummy.org"}
              "In 2101, war was beginning. What happen? Main screen turn on. For great justice.")]
        ["constrict by nearest line" "xcl:dummy.org?line=support+scheduled"
         (RR. :exact-name
              :line-with-match
              {:file-name "dummy.org"}
              "Support attributes like ~SCHEDULED:~.")]
        ["constrict by first matching paragraph" "xcl:dummy.org?para=what+happen"
         (RR. :exact-name
              :paragraph-with-match
              {:file-name "dummy.org"}
              "In 2101, war was beginning. What happen? Main screen turn on. For great justice. Move ZIG.")]
        ["constrict by first matching section" "xcl:dummy.org?section=famous+script"
         (RR. :exact-name
              :org-section-with-match
              {:file-name "dummy.org"}
              "** famous script\n\n   Captain and CATS\n   \n   In 2101, war was beginning. What happen? Main screen turn on. For great justice. Move ZIG."
              )]
        ]
       (map
        (fn [[desc link expected]]
          (let [received (sc/parse-link
                          (fn [& _]
                            (corpus/list-files "_test"))
                          get-static-content
                          link)
                success? (->> (keys expected)
                              (map (fn [k]
                                     (let [is-equal? (= (k received)
                                                        (k expected))]
                                       is-equal?)))
                              (every? identity))]
            [:tr
             [:td
              {:style {:background-color
                       (if success? "lime" "red")
                       :color "white"}}
              (if success? "PASS" "FAIL")]
             [:td desc]
             [:td [:code link]]
             [:td (render-map expected)]
             [:td (render-map received)]])))
       (concat [:tbody
                [:tr
                 [:th "PASS?"]
                 [:th "description"]
                 [:th "link expression"]
                 [:th "expected"]
                 [:th "resolves to"]]])
       (vec)
       (vector :table
               {:style {:border-collapse "collapse"}}
               [:style "td { border: 1px solid gray; }"])
       (vec)))

(defn render-transclusion-test-view! []
  (let [textarea (fn [content]
                   [:textarea
                    {:style {:width "20em"
                             :height "20em"}}
                    content])]
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
                   (:path xcl-spec)
                   " :lines "
                   (get-in xcl-spec
                           [:content-boundary :beg])
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
                  [:tr
                   [:td
                    {:style {:color "white"
                             :background (if is-same?
                                           "lime"
                                           "red")}}
                    [:b source-file]]
                   [:td (textarea source-text)]
                   [:td (textarea expected)]
                   [:td (textarea rendered)]])))
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
         (vec))))

(defn main []
  (r/render
   [:div
    [:h2 "link test view"]
    [:div (render-link-test-view!)]
    ;; [:div {:style {:width "100%"
    ;;                :border "1px solid blue"}}
    ;;  (render-transclusion-test-view!)
    ;;  [:div {:style {:clear "both"}}]]
    ]
   (gdom/getElement "main-app"))
  )

(main)

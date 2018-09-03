(ns xcl.viz
  (:require [reagent.core :as r]
            [goog.dom :as gdom]
            [xcl.core :as sc]
            [xcl.corpus :as corpus]))

(defn render-map [m]
  {:pre [(map? m)]}
  (->> m
       (map (fn [[k v]]
              [:tr
               [:th [:code k]]
               [:td [:code (pr-str v)]]]))
       (concat [:tbody])
       (vec)
       (vector :table {:style {:font-size "x-small"}})
       (vec)))

(defn render-link-test-view! [target-el]
  ;; resolver record
  (defrecord RR
      [resource-resolver
       content-resolver
       file-name
       match-content])

  (let []
    (r/render
     (->> [["full file" "tiny.org"
            (RR. :exact-name
                 :whole-file
                 "tiny.org"
                 (corpus/load-content "tiny.org"))]
           ["full file" "file:tiny.org"
            (RR. :exact-name
                 :whole-file
                 "tiny.org"
                 (corpus/load-content "tiny.org"))]
           ["line in file" "LICENSE::7"
            (RR. :exact-name
                 :line-range
                 "LICENSE"
                 "of this license document, but changing it is not allowed.")]
           ["line in file" "file:100lines::5"
            (RR. :exact-name
                 :line-range
                 "100lines"
                 "5 SOME LINE")]
           ["line range" "file:tiny.org::2-3"
            (RR. :exact-name
                 :line-range
                 "tiny.org"
                 "* decoy 1\n* something third line")]
           ["line from start" "file:tiny.org::-2"
            (RR. :exact-name
                 :line-range
                 "tiny.org"
                 "fake file (line 1)\n* decoy 1")]
           ["line to end" "file:tiny.org::7-"
            (RR. :exact-name
                 :line-range
                 "tiny.org"
                 "seven 7s\nocho acht")]
           ["character range" "tiny.org::5,40"
            (RR. :exact-name
                 :char-range
                 "tiny.org"
                 "file (line 1)\n* decoy 1\n* something")]
           ["character from start" "file:tiny.org::,20"
            (RR. :exact-name
                 :char-range
                 "tiny.org"
                 "fake file (line 1)\n*")]
           ["character to end" "file:tiny.org::75,"
            (RR. :exact-name
                 :char-range
                 "tiny.org"
                 "h line\nsix sixths is sick sith\nseven 7s\nocho acht")]
           ["percent range" "100lines::1%-3%"
            (RR. :exact-name
                 :percent-range
                 "100lines"
                 "2 SOME LINE\n3 SOME LINE\n4 SOME LINE")]
           ["native org: heading" "file:tiny.org::*decoy 1"
            (RR. :exact-name
                 :org-heading
                 "tiny.org"
                 "* decoy 1")]
           ["exact string match range" "file:dummy.org::in 2101...for great justice"
            (RR. :exact-name
                 :token-bound
                 "dummy.org"
                 "In 2101, war was beginning. What happen? Main screen turn on. For great justice")]
           ["glob file name" "file:d*y.org::*huh"
            (RR. :glob-name
                 :org-heading
                 "dummy.org"
                 "* huh\n\nwhatever is in the block")]
           ["fuzzy find file by content +" "grep:ZZ+you::*huh"
            (RR. :grep-content
                 :org-heading
                 "dummy.org"
                 "* huh\n\nwhatever is in the block")]
           ["fuzzy find file by content raw space" "grep:ZZ you::*huh"
            (RR. :grep-content
                 :org-heading
                 "dummy.org"
                 "* huh\n\nwhatever is in the block")]
           ["fuzzy find file by html entity" "grep:ZZ%20you::*huh"
            (RR. :grep-content
                 :org-heading
                 "dummy.org"
                 "* huh\n\nwhatever is in the block")]
           ["constrict by org node ID" "xcl:dummy.org?id=my-node-id"
            (RR. :exact-name
                 :org-node-id
                 "dummy.org"
                 "* next heading\n  :PROPERTIES:\n  :CUSTOM_ID: my-node-id\n  :END:\n\n  good stuff")]
           ["constrict by first token range" "xcl:dummy.org?s=in 2101...for great justice."
            (RR. :exact-name
                 :token-bound
                 "dummy.org"
                 "In 2101, war was beginning. What happen? Main screen turn on. For great justice.")]
           ["constrict by nearest line" "xcl:dummy.org?line=support+scheduled"
            (RR. :exact-name
                 :line-with-match
                 "dummy.org"
                 "Support attributes like ~SCHEDULED:~.")]
           ["constrict by first matching paragraph" "xcl:dummy.org?para=what+happen"
            (RR. :exact-name
                 :paragraph-with-match
                 "dummy.org"
                 "In 2101, war was beginning. What happen? Main screen turn on. For great justice. Move ZIG.")]
           ["constrict by first matching section" "xcl:dummy.org?section=famous+script"
            (RR. :exact-name
                 :org-section-with-match
                 "dummy.org"
                 "** famous script\n\n   Captain and CATS\n   \n   In 2101, war was beginning. What happen? Main screen turn on. For great justice. Move ZIG."
                 )]
           ]
          (map
           (fn [[desc link expected]]
             (let [received (sc/parse link)
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
          (vec))
     target-el)))

(defn main []
  (render-link-test-view!
   (gdom/getElement "main-app")))

(main)

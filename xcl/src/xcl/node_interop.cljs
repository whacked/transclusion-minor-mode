(ns xcl.node-interop
  (:require [xcl.common :refer [get-file-extension]]
            [xcl.core :as sc :refer [render-transclusion]]
            [xcl.content-interop :as ci]
            [xcl.external :as ext]
            [xcl.pdfjslib-interop
             :refer [pdfjslib-load-text
                     set-pdfjslib!]
             :as pdfi]
            ["pdfjs-dist" :as pdfjsLib]
            [xcl.node-epub-interop :as epubi]
            ["fs" :as fs]
            ["path" :as path]
            ["js-yaml" :as yaml]
            ["JSONPath" :as JSONPath]
            [xcl.node-common :refer
             [path-exists? path-join]]))

;;;;;;;;;;;;;;;
;; epub, pdf ;;
;;;;;;;;;;;;;;;
(set-pdfjslib! pdfjsLib)
(ext/register-loader!
 "pdf"
 (fn [resource-address callback]
   (let [file-name (:resource-resolver-path resource-address)]
     (if-not file-name
       (js/console.warn (str "NO SUCH FILE: " file-name))
       (let [maybe-page-number-bound
             (ci/get-maybe-page-number-bound resource-address)]
         (let [rel-uri (str file-name)]
           (pdfjslib-load-text
            rel-uri
            (:beg maybe-page-number-bound)
            (:end maybe-page-number-bound)
            callback)))))))

(ext/register-loader!
 "epub"
 (fn [resource-address callback]
   (let [file-name (:resource-resolver-path resource-address)]
     (if-not file-name
       (js/console.warn (str "NO SUCH FILE: " file-name))
       (let [maybe-page-number-bound
             (ci/get-maybe-page-number-bound resource-address)]
         (epubi/load-and-get-text
          file-name
          (:beg maybe-page-number-bound)
          (:end maybe-page-number-bound)
          nil
          (fn [chapters]
            (->> chapters
                 (map :text)
                 (interpose " ")
                 (apply str)
                 (callback)))))))))

(defn render
  "compatibility function for calling from nodejs; wraps all fns in
  postprocessor-coll to ensure resultant object is native #js type
  "
  [candidate-seq-loader content-loader source-text & postprocessor-coll]
  
  (->> postprocessor-coll
       (map (fn [postprocessor-fn]
              (fn [content xcl-spec depth]
                (postprocessor-fn content (clj->js xcl-spec) depth))))
       (apply render-transclusion
              candidate-seq-loader content-loader source-text)))

(defn get-exact-text-from-partial-string-match
  [file-path match-string on-complete]
  (let [search-tokens (clojure.string/split match-string #"\s+")
        result (atom nil)]
    (case (get-file-extension file-path)
      "epub" (epubi/load-and-get-text
              file-path
              nil
              nil
              (fn on-get-section [{:keys [section chapter text]}]
                (let [maybe-matches (sc/find-most-compact-token-matches-in-content
                                     text search-tokens)]
                  (when (= (count maybe-matches)
                           (count search-tokens))
                    (let [first-match (first maybe-matches)
                          last-match (last maybe-matches)
                          index-beg (:index first-match)
                          index-end (+ (:index last-match)
                                       (:length last-match))]
                      (swap! result assoc
                             :section section
                             :chapter chapter
                             :excerpt (subs text index-beg index-end))))))
              (fn on-complete-all-sections [_sections]
                (on-complete (clj->js @result))))
     
      "pdf" (pdfi/process-pdf
             file-path
             (fn on-get-page-text [page-num page-text]
               (let [maybe-matches (sc/find-most-compact-token-matches-in-content
                                    page-text search-tokens)]
                 (when (= (count maybe-matches)
                          (count search-tokens))
                   (let [first-match (first maybe-matches)
                         last-match (last maybe-matches)
                         index-beg (:index first-match)
                         index-end (+ (:index last-match)
                                      (:length last-match))]
                     (swap! result assoc
                            :page page-num
                            :excerpt (subs page-text index-beg index-end))))))
             (fn on-complete-page-texts [page-texts]
               (on-complete (clj->js @result))))
     
      nil)))


(def yaml-loader
  (fn [resource-address callback]
    (let [file-name (:resource-resolver-path resource-address)]
      (if-not file-name
        (js/console.warn (str "NO SUCH FILE: " file-name))
        (let [source (.readFileSync fs file-name "utf-8")
              parsed (.safeLoad yaml source)]
          (callback parsed))))))

;;;;;;;;;;
;; yaml ;;
;;;;;;;;;;
(ext/register-loader! "yml" yaml-loader)
(ext/register-loader! "yaml" yaml-loader)

;;;;;;;;;;
;; json ;;
;;;;;;;;;;
(ext/register-loader!
 "json"
 (fn [resource-address callback]
   (let [file-name (:resource-resolver-path resource-address)]
     (if-not file-name
       (js/console.warn (str "NO SUCH FILE: " file-name))
       (let [source (.readFileSync fs file-name "utf-8")
             parsed (.parse js/JSON source)]
         (callback parsed))))))

;;;;;;;;;;;;;;;;;;;;;;;
;; jsonpath resolver ;;
;;;;;;;;;;;;;;;;;;;;;;;
(swap! ci/$resolver
       assoc :jsonpath
       ;; NOTE: relying on yaml to parse json AND yaml
       (fn [bound-spec content]
         (let [jsonpath (get-in bound-spec
                                [:bound :jsonpath])]
           (some-> content
                   (ext/read-yaml)
                   (ext/read-jsonpath-content jsonpath)
                   (first)))))

;;;;;;;;;;;;;;;;;;;;;
;; calibre, zotero ;;
;;;;;;;;;;;;;;;;;;;;;
(swap! sc/$known-protocols
       conj :calibre :zotero)

;; TODO: need to clarify this; may be redundant
(swap! sc/$known-resource-resolver-mapping
       assoc
       :calibre
       :calibre-file)

;; TODO: need to clarify this; may be redundant
(swap! sc/$known-resource-resolver-mapping
       assoc
       :zotero
       :zotero-file)

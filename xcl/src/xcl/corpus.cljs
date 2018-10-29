(ns xcl.corpus
  (:require-macros [xcl.static-loader :as loader]))

(def file-cache
  {"LICENSE" (loader/slurp-file "LICENSE")
   "100lines" (->> (range 100)
                   (map (fn [i]
                          (str (inc i) " SOME LINE\n")))
                   (apply str))
   "_UNLICENSE_" "foo bar\nbaz qux"
   "tiny.org" (->> ["fake file (line 1)"
                    "* decoy 1"
                    "* something third line"
                    "fourth line"
                    "1 2 3 4, 5th line"
                    "six sixths is sick sith"
                    "seven 7s"
                    "ocho acht"]
                   (interpose "\n")
                   (apply str))
   "big.org" "another fake file"
   "fake.org" "some fake information\nto throw you off"
   "dummy.org" (loader/slurp-file "xcl/public/dummy.org")
   "_READ.ME.org" "decoy org file"
   "README.org" (loader/slurp-file "README.org")
   "xcl-test-1.org" (->> ["* blah blah"
                          ""
                          "generic content"
                          "genetic content"
                          "{{{transclude(big.org)}}}"
                          ""
                          "{{{transclude(dummy.org::*huh)}}}"
                          ]
                         (interpose "\n")
                         (apply str))
   "xcl-test-2.org" (->> ["* fake file 2"
                          ""
                          "random block"
                          "tandem block"
                          "{{{transclude(100lines::5-7)}}}"
                          ""
                          "{{{transclude(xcl:dummy.org?para=what+happen)}}}"
                          ]
                         (interpose "\n")
                         (apply str))
   "xcl-test-3-a.org" "content of A!\n\naye aye aye"
   "xcl-test-3-b.org" "* I am B and I include A\n\n** {{{transclude(xcl:xcl-test-3-a.org)}}}"
   "xcl-test-3-c.org" "* I am C and I include B\n\n*{{{transclude(xcl:xcl-test-3-b.org)}}}"
   "xcl-test-3-d.org" "* I am D and I include A\n\n{{{transclude(xcl:xcl-test-3-a.org::1)}}}"
   "xcl-test-self-recursion.org" "I include myself:\n{{{transclude(xcl:xcl-test-self-recursion.org)}}}"
   "xcl-test-infinite-1.org" "Hi from 1. I include infinite 2:\n{{{transclude(xcl:xcl-test-infinite-2.org)}}}"
   "xcl-test-infinite-2.org" "Hi from 2. I include infinite 1:\n{{{transclude(xcl:xcl-test-infinite-1.org)}}}"
   })

(defn list-files [_fake-directory]
  (keys file-cache))

(defn load-content [filename]
  (file-cache filename))

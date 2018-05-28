;; in namespace declarations:

(ns my-ns
  (:require [scratchpad.bla :as bla]))

;; ---------------------------------------

(require 'clojure.set)
(prn (clojure.set/union #{1 2} #{3 4}))
(prn)

;; ----------------------------------------

(require '[clojure.set :as set])
(prn (set/union #{1 2} #{3 4}))
(prn)

;; ----------------------------------------

(require '(clojure [string :as str]
                   [set :as set]))
(prn (str/join ", " ["one" "two"]))
(prn)

;; ----------------------------------------

(use '(clojure [string :only (join) :as str]
               [set :exclude (join)]))
(prn (join ["one" "two"]))
(prn (str/upper-case "bla"))
(prn (intersection #{1 2} #{2 3}))

;; ---------------------------------------
;; reloading dependencies:

(require '[clojure.set :as set] :reload) ;; reloads set only
(require '[clojure.set :as set] :reload-all) ;; reloads all its dependencies as well

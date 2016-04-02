(require 'clojure.set)
(clojure.set/union #{1 2} #{3 4})

(require '[clojure.set :as set])
(set/union #{1 2} #{3 4})

(require '(clojure [string :as str]
                   [set :as set]))
(str/join ", " ["one" "two"])

(use '(clojure [string :only (join) :as str]
               [set :exclude (join)]))
(join ["one" "two"])
(str/upper-case "bla")
(intersection #{1 2} #{2 3})

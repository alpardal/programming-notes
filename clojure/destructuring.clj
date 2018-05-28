
(defn something [{:keys [a b] :as args :or {b 2}}]
  {:a a :b b :args args})

;; prints `{:a 1, :b 2, :args {:a 1, :c 3}}`
(println (something {:a 1 :c 3}))

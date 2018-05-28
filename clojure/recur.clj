
(defn fact [n]
  (loop [cur n accu 1]
    (if (zero? cur)
      accu
      (recur (dec cur) (* accu cur)))))

(println (fact 5))

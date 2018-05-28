(ns test)

(defmulti Action
  (fn [state action]
    (:type action)))

(defmethod Action :default [state {:keys [type] :as action-data}]
  (printf "Action of type '%s' not defined, using default.%n" type)
  state)

(defmethod Action :bla [state _]
  (println "Dispatching a lovely bla.")
  state)

(Action 1 {:type :bla})
(Action 1 {:type "unknown"})

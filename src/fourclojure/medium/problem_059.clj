(ns fourclojure.medium.problem-059)

;; Juxtaposition
;; https://www.4clojure.com/problem/59
(defn prob59 [& funcs]
  (fn [& args]
    (map #(apply % args) funcs)
    ))



(ns fourclojure.medium.problem-158)

;; Decurry
;; https://www.4clojure.com/problem/158

(fn [f]
  (fn [& args]
    (reduce #(%1 %2) f args)))


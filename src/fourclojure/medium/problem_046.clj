(ns fourclojure.medium.problem-046)

;; Flipping out
;; https://www.4clojure.com/problem/46
;; return functino that flips the order of the aruments of the input function
(defn prob46 [f]
  (fn [x y]
    (f y x)))


(ns fourclojure.medium.problem-080)

;; Perfect Numbers
;; https://www.4clojure.com/problem/80
;; a number is perfect if the sum of its divisors equal the number itself
(defn prob80
  [num]
  (letfn [(divisors [n]
            (filter #(zero? (mod n %)) (range 1 (+ 1 (/ n 2))))
            )]
    (= (reduce + (divisors num)) num)))


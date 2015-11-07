(ns fourclojure.medium.problem-075)

;; Euler's Totient Function
;; https://www.4clojure.com/problem/75
;; Two numbers are coprime if their greatest common divisor equals 1.
;; Euler's totient function f(x) is defined as the number of positive integers 
;; less than x which are coprime to x.
(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (mod a b))))

(defn coprime [a b]
  (= 1 (gcd a b)))

;; number of positive integers less than x
(defn prob75
  [x]
  (cond
   (= x 1) 1
   :else (count (filter #(coprime % x) (range x)))))

(defn prob75
  [x]
  (cond
   (= x 1) 1
   :else (letfn [(gcd [a b] 
                   (if (zero? b)
                     a
                     (gcd b (mod a b))))
                 (coprime [a b]
                   (= 1 (gcd a b)))
                 ]
           (count (filter #(coprime % x) (range x))))))




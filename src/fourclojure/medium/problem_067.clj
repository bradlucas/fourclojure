(ns fourclojure.medium.problem-067)

;; Primes
;; https://www.4clojure.com/problem/67
(defn prob67 [n]
  (take n
        (cons 2 (filter 
                 (fn [x] ;; is prime
                   (empty? (filter #(= 0 (mod x %)) (range 2 x)))
                   )
                 (iterate #(+ 2 %) 3)))))

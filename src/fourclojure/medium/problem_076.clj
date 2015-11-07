(ns fourclojure.medium.problem-076)

;; Intro to Trampoline
;; https://www.4clojure.com/problem/76
(= [1 3 5 7 9 11]
   (letfn
       [(foo [x y] #(bar (conj x y) y))
        (bar [x y] (if (> (last x) 10)
                     x
                     #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))



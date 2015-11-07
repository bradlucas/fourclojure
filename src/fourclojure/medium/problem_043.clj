(ns fourclojure.medium.problem-043)

;; Reverse Interleave
;; https://www.4clojure.com/problem/43
(defn prob43 [col n]
  (apply map list (partition n col)))

(= (prob43 [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
(= (prob43 (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
(= (prob43 (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))


(ns fourclojure.medium.problem-044)

;; Rotate Sequence #44
;; https://www.4clojure.com/problem/44
(defn prob44 [n s]
  (let [len (count s)
        idx (cond 
             (< n 0) (+ len (mod n (- len)))
             (> n len) (mod n len)
             :else n)]
    (lazy-cat (drop idx s) (take idx s))))

;; (= (prob44 2 [1 2 3 4 5]) '(3 4 5 1 2))
;; (= (prob44 -2 [1 2 3 4 5]) '(4 5 1 2 3))
;; (= (prob44 6 [1 2 3 4 5]) '(2 3 4 5 1))
;; (= (prob44 1 '(:a :b :c)) '(:b :c :a))
;; (= (prob44 -4 '(:a :b :c)) '(:c :a :b))

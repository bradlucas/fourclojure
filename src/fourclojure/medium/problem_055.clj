(ns fourclojure.medium.problem-055)

;; Count Occurrences
;; https://www.4clojure.com/problem/55
(defn prob55 [col]
  (loop[ret {}
        col col]
    (if (empty? col)
      ret
      (recur 
       (assoc ret (first col) (inc (get ret (first col) 0)))
       (rest col)))))


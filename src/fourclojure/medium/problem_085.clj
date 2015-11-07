(ns fourclojure.medium.problem-085)

;; Power Set
;; https://www.4clojure.com/problem/85
(defn prob85
  [xs]
  (set (map set 
            (if (empty? xs) 
              '(())
              (clojure.set/union (prob85 (next xs))
                                 (set (map #(conj % (first xs)) (prob85 (rest xs)))))
              ))))



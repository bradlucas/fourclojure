(ns fourclojure.medium.problem-056)

;; Find Distinct Items
;; https://www.4clojure.com/problem/56
(defn prob56 [s]
  (loop [acc []
         col s]
    (if (empty? col)
      acc
      (recur 
       (if (not (some #(= (first col) %) acc))
         (conj acc (first col))
         acc
         )
       (rest col)))))


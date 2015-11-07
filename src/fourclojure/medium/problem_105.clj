(ns fourclojure.medium.problem-105)

;; Identify keys and values
;; https://www.4clojure.com/problem/105
(defn prob105 [s]
  (loop [s s
         prev nil
         ret {}]
    (if (empty? s)
      ret
      (let [f (first s)
            key (if (keyword? f) f prev)
            prev key
            val (ret key nil)
            ]
        (recur (rest s) key (assoc ret key (if (keyword? f) [] (conj (ret key) f))))
        ))))


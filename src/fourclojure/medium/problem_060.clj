(ns fourclojure.medium.problem-060)

;; Squence Reductions
;; https://www.4clojure.com/problem/60
(defn prob60
  ([f coll] 
   (prob60 f (first coll) (rest coll)))
  ([f v coll] 
   (if (seq coll)
     (let [next-v (f v (first coll))
           next-coll (rest coll)
           ]
       (cons v (lazy-seq (prob60 f next-v next-coll)))
       )
     (cons v (lazy-seq '())))))


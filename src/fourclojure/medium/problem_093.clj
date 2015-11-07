(ns fourclojure.medium.problem-093)

;; Partially Flatten a Sequence
;; https://www.4clojure.com/problem/93

;; [["Do"] ["Nothing"]]
;; [[[[:a :b]]] [[:c :d]] [:e :f]] => [[:a :b] [:c :d] [:e :f]]

(defn prob93 [coll]
  (when-let [s (seq coll)]
    (if (and (coll? (first s)) (coll? (first (rest s))))
      (concat (prob93 (first s)) (prob93 (rest s)))
      (cons (first s) (flatten (rest s))))))

;; https://github.com/khotyn/4clojure-answer/blob/master/93-partially-flatten-a-sequence.clj
(defn prob93
  ([coll] (prob93 coll []))
  ([coll result]
   (if (seq coll)
     (let [first-item (first coll)]
       (prob93 (next coll)
               (if (some coll? first-item)
                 (reduce #(conj %1 %2) result (prob93 first-item []))
                 (conj result first-item))))
     result
     )
   )
  )



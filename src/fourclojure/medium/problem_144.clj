(ns fourclojure.medium.problem-144)

;; Oscilrate
;; https://www.4clojure.com/problem/144

;; non-lazy version which doesn't work if n is > than the number of fns
(defn prob144
  [n & fns]
  (loop [ops fns
        acc [n]]
    (if (empty? ops)
      acc
      (let [val (last acc)]
        (recur (rest ops) (conj acc ((first ops) val)))))))

(defn reduce-with-steps
  ([f coll]
   (lazy-seq
    (if-let [s (seq coll)]
             (reduce-with-steps f (first s) (rest s))
             (list (f)))))
  ([f init coll]
   (cons init
         (lazy-seq
          (when-let [s (seq coll)]
            (reduce-with-steps f (f init (first s)) (rest s)))))
   ))

(defn prob144
  [n & fs]
  (reduce-with-steps #(%2 %1) n (cycle fs)))

(defn prob144
  [n & fs]
  (reductions (fn [v f] (f v)) n (cycle fs)))



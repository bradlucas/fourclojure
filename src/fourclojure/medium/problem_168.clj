(ns fourclojure.medium.problem-168)


;; Inifinite Matrix
;; https://www.4clojure.com/problem/168

;; https://github.com/khotyn/4clojure-answer/blob/master/168-infinite-matrix.clj
(defn prob168
  ;; return infinite matrix A
  ([f] (prob168 f 0 0))

  ;; return matrix A with m rows and n columns removed
  ([f m n] 
   (letfn [(rows [i j]
             (lazy-seq (cons (f i j) (rows i (inc j)))))
           (cols [i]
             (lazy-seq (cons (rows i n) (cols (inc i)))))]
     (cols m)))

  ;; return finite s-by-t matrix with first t entires of each first s rows of matrix b
  ([f m n s t]
   (take s (map #(take t %) (prob168 f m n))))
  )





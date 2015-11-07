(ns fourclojure.medium.problem-132)

;; Insert between two items
;; https://www.4clojure.com/problem/132
;; 
;; Return seq with val 

(defn prob132 
  ([pred? val seq] (prob132 pred? val (seq seq) []))
  ([pred? val seq res]
   (if (not (empty? seq))
     (let [pred? pred?
           val val]
       (loop [res (conj res (first seq))
              seq (rest seq)]
         (let [head (first seq)]
           (println res head seq)
           (if (nil? head)
             res
             (if (pred? (last res) head)
               (recur (conj res val head)(rest seq))
               (recur (conj res head) (rest seq) )
               )
             )
           )
         )
       )
     []
     )
   )
  )

;; http://stackoverflow.com/questions/7831109/idomatically-insert-items-between-two-items-in-a-sequence-that-fulfill-a-predica

;; working
(defn prob132 [pred val [a b & _ :as s]]
  (lazy-seq
        (when-not (empty? s)
          (if (and b (pred a b))
            (cons a (cons val (prob132 pred val (rest s))))
            (cons a (prob132 pred val (rest s)))
            )))
)


;; [0 1 :same 1 2 3 :same 5 8 13 :same 21]
(take 12 (->> [0 1]
              (iterate (fn [[a b]] [b (+ a b)]))
              (map first) ; fibonacci numbers
              (prob132 (fn [a b] ; both even or both odd
                       (= (mod a 2) (mod b 2)))
                     :same)
              )              
              )


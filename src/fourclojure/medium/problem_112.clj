(ns fourclojure.medium.problem-112)

;; Sequs Horribilis
;; https://www.4clojure.com/problem/112

;; return sequence with same nested structure
;; include elements from head that are less than or equal to the input integer
;; ie, flatten and remove brackets and sum elements and include only up to 

;; (prob112 10 [1 2 [3 [4 5] 6] 7]) => '(1 2 (3 (4))))  


;; chouser's solution
;; http://pastebin.com/Pfa0y9va

(defn prob112 [limit xs]
  (second 
   ((fn f [[limit] xs]
      (if (coll? xs)
        (let [rs (take-while #(<= 0 (first %))
                             (reductions f [limit] xs))]
          [(first (last rs)) (map second (rest rs))])
        [(- limit xs) xs])
      )
    [limit] xs)))


(defn prob112-helper [[num] xs]
  (if (coll? xs)
    (let [rs (take-while #(<= 0 (first %))
                         (reductions prob112-helper [num] xs))]
      [(first (last rs)) (map second (rest rs))])
    [(- num xs) xs]))


(defn prob112
  [num xs]
  (second (prob112-helper [num] xs))
)

(defn prob112
  [num xs]
  (letfn [(prob112-helper [[num] xs]
            (if (coll? xs)
              (let [rs (take-while #(<= 0 (first %))
                                   (reductions prob112-helper [num] xs))]
                [(first (last rs)) (map second (rest rs))])
              [(- num xs) xs]))]
    (second (prob112-helper [num] xs))))



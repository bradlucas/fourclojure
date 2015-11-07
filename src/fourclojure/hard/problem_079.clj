(ns fourclojure.hard.hard.problem-079)

;; Triangle Minimal Path
 ;; 79

;; find the cheapest path through the triangle
;; (= 7 (__ '([1]
;;           [2 4]
;;          [5 1 4]
;;         [2 3 4 5]))) ; 1->2->1->3


;; breadth-first build all paths
;; Didn't finish....
;;
;; (defn process-node [n]
;;   (if (empty? n) nil
;;       (cons (first n) (process-node (rest n)))))

;; (defn prob79
;;   [t]
;;   (letfn [(process-node [n & rest]
;;             (if (empty? n) nil
;;                 (cons (first n) (process-node (rest n) rest))))
;;           (process-rest []
;;             )
;;           ]
;;     (if (empty? t)
;;       nil
;;       (process-node (first t) (rest t))
;; )))

;; ;; for each vector in xs
;; ;;    for each element in vector
;; ;;       (cons element (fn (rest xs)

;; (defn node-values [nodes]
;;   (map first nodes))

;; (defn node-children [nodes]
;;   ;;(mapcat next nodes)
;;   (map next nodes))

;; (defn depth-traversal [nodes]
;;   (if (not (empty? nodes))
;;     (interleave (node-values nodes) (node-children nodes))))



;; [1]      => [1 1]   => (fn [])  => 5
;; [2 4]       [2 4]

;; [2 4]       [
;; [5 1 4]     [5 1 4]

;; reverse tree to the above is reversed

;; [2 4]       [2 4]
;; [1]      => [1 1]   => (fn [])  => 5


;; [5 1 4]     [5 1 1 4]
;; [2 4]       [2 2 4 4]


;; [2 4]       [2 4]
;; [1]      => [1 1]   => (fn [])  => 5


;; reverse the triangle
;; realize that for each pair at the bottom row
;; one is <> then the other and a result should be picked
;; add these to the pairs above

;; final row
;; for each pair select the max
;; add this to the previous row
;; repeat


(defn pair-row
  [row]
  (partition 2 1 row))

(defn min-pair-row
  [paired-row]
  (map #(reduce min %) paired-row))

(defn reduce-rows-min
  [row1 row2]
  (map + (min-pair-row (pair-row row1)) row2))

(defn run-min
  [t]
  (first (reduce reduce-rows-min (reverse t))))

;; Realization
;; Bottom-up is more tractable
;; The bottom row paired are the results of the choice from above
;; Pick the smallest of each pair and sum with the previous row
;; Then continue


;; Work through the tree from bottom up (reverse the tree)
;; pair-row partitions the row into pairs [5 1 4] => ((5 1) (1 4))
;; min-pair-row reduces the paired row to smallest values for each pair
;; add the min-paired-row to the previous
;; continue throught the rest of the reversed tree

(defn prob79
  [t]
  (letfn [(pair-row
            [row]
            (partition 2 1 row))
          (min-pair-row
            [paired-row]
            (map #(reduce min %) paired-row))
          (reduce-rows-min
            [row1 row2]
            (map + (min-pair-row (pair-row row1)) row2))]
    (first (reduce reduce-rows-min (reverse t)))))


;; euler 18
;; (defn max-pair-row
;;   [row]
;;   (map #(reduce max %) (pair-row row)))

;; (defn reduce-rows-max
;;   [row1 row2]
 ;;   (map + (max-pair-row row1) row2))

;; (defn run-max
;;   [t]
;;   (first (reduce reduce-rows (reverse t))))

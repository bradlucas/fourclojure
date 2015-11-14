(ns fourclojure.hard.problem-053)

;; Longest Increasing Sub-Seq
;; http://www.4clojure.com/problem/53
(defn prob53 [s]
  (->> (partition 2 1 s)
       (partition-by (fn [[p1 p2]] (> p2 p1)))   ;; grouped by results of function 

       ;; ( ((1 0)) ((0 1) (1 2) (2 3)) ((3 0)) ((0 4) (4 5)) )

       ;; pull out the groups into new sequences
       ;; a. map first of each element into a vector gets the first of teach subgroup
       ;;  ([1] [0 1 2] [3] [0 4]) 
       ;; b. need to pick up the very last item
       ;; ([1 0] [0 1 2 3] [3 0] [0 4 5])
       (map 
        #(conj
          (into [] (map first %)) (last (last %)))
        )

       (filter (fn [[a b]] (> b a)))    ;; pick the ones with increasing vlaues
       ;; ([0 1 2 3] [0 4 5])

       (reduce (fn [a b] (if (>  (count b) (count a)) b a)) [])  ;; pick the longest
       ;; [0 1 2 3]
       ))


(def tests 
  [
   [1 0 1 2 3 0 4 5]
   [5 6 1 3 2 7]
   [2 3 3 4 5]
   [7 6 5 4]])

(map prob53 tests)





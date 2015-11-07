(ns fourclojure.medium.problem-171)


;; Intervals
;; https://www.4clojure.com/problem/171

;; sort
;; find embedded continuous sequences
;; for each return the star and end of each
;; [10 9 8 1 2 3] => [[1 3] [ 8 10]]
(defn prob171
  [xs]
  (let [sorted (sort (seq xs))            ;; remove duplicates and sort
        pairs (map list sorted (range))   ;; build a list of pairs where you have a value and a position count
        ;; the difference between the value and the index will be the same for each group
        groups (map #(apply - %) pairs)
        ;; so we want to partition-by where this value changes
        grouped (partition-by #(apply - %) pairs)]
    (println "Input   ", xs)
    (println "Sorted  ", sorted)
    (println "Pairs   ", pairs)
    (println "Groups  ", groups)
    (println "Grouped ", grouped)
    ;; pull out the values and leave the indexes
    ;; (map #(map first %) grouped)
    ;; pull out the first value and the last value of each group
    (map #(vector (ffirst %) (first (last %))) grouped)))


(defn prob171
  [xs]
  (let [sorted (sort (set xs))                            ;; sort and remove duplicates
        pairs (map list sorted (range))                   ;; pair values with a count
        grouped (partition-by #(apply - %) pairs)]        ;; split where the diff in the pair changes
    (map #(vector (ffirst %) (first (last %))) grouped))) ;; pull out the grouped values

(= (prob171 [10 9 8 1 2 3]) [[1 3] [8 10]])



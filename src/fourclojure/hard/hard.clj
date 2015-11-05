(ns fourclojure.hard.hard
  (:require clojure.data))

;; Read Roman numerals
;; Transitive Closure
;; Graph Connectivity
;; Game of Life	Hard
;; Number Maze	Hard
;; Levenshtein Distance
;; Graph Tour
;; For Science!
;; Win at Tic-Tac-Toe
;; Making Data Dance
;; Crossword puzzle
;; Gus' Quinundrum
;; Best Hand
;; Analyze Reversi
;; Tree reparenting
;; Squares Squared
;; Language of a DFA
;; Love Triangle
;; Veitch, Please!
;; Latin Square Slicing



;; Longest Increasing Sub-Seq
;; 53
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





;; Analyze a Tic-Tac-Toe Board
;; #73
(def board [[:x :e :o]
            [:x :e :e]
            [:x :e :o]])

(defn row [m n] (nth m n))

(defn rows [m]
  (map #(row m %) (range 3)))

(defn col [m n] (vec (map #(nth % n) m)))

(defn cols [m]
  (map #(col m %) (range 3)))

(defn diags [m] 
  (list [(last (col m 0)) (second (col m 1)) (first (col m 2))]
        [(first (col m 0)) (second (col m 1)) (last (col m 2))]))

(defn triples [m]
  (concat (rows m) (cols m) (diags m)))
           
(defn all-same? [s]
  (let [v (first s)]
    (if (every? (fn [x] (= x v)) s)
      v
      nil)))

(defn prob73 [m]
  ;; return first path that contains all :x or :o
  ;; if none return nil
  (let [tups (triples m)]
    (if (filter #(= [:x :x :x] %) tups)
      :x
      (if (filter #(= [:o :o :o] %) tups)
      :o
      nil
        )
      )
    )
)

(= :x (prob73 board))


(defn prob73 [m]
  ;; return first path that contains all :x or :o
  ;; if none return nil
  (letfn [(triples [m]
            (concat (rows m) (cols m) (diags m)))
          (rows [m]
            (map #(row m %) (range 3)))
          (row [m n] (nth m n))
          (cols [m]
            (map #(col m %) (range 3)))
          (col [m n] (vec (map #(nth % n) m)))
          (diags [m] 
            (list [(last (col m 0)) (second (col m 1)) (first (col m 2))]
                  [(first (col m 0)) (second (col m 1)) (last (col m 2))]))]
  (let [tups (triples m)]
    (print tups)
    (print (count (filter #(= [:x :x :x] %) tups)))
    (if (> (count (filter #(= [:x :x :x] %) tups)) 0)
      :x
      (if (> (count (filter #(= [:o :o :o] %) tups)) 0)
        :o
        nil
        )))))


(= nil (prob73 [[:e :e :e]
                [:e :e :e]
                [:e :e :e]]))

(= :x (prob73 [[:x :e :o]
               [:x :e :e]
               [:x :e :o]]))

(= :o (prob73 [[:e :x :e]
               [:o :o :o]
               [:x :e :x]]))

(= nil (prob73 [[:x :e :o]
            [:x :x :e]
            [:o :x :o]]))

(= :x (prob73 [[:x :e :e]
           [:o :x :e]
           [:o :e :x]]))

(= :o (prob73 [[:x :e :o]
           [:x :o :e]
           [:o :e :x]]))

(= nil (prob73 [[:x :o :x]
            [:x :o :x]
            [:o :x :o]]))







;; #82
;; can the input words be arranged so they differ by one character
;; cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog
;;
;; build all the possible lists of words
;;
;; is there one where each differs by one character
;; (= (word-chain "cat", "cot", "coat", "oat", "hat", "hot", "hog", "dog"))
;; http://www.4clojure.com/problem/82#prob-title
;; http://codekata.com/kata/kata19-word-chains/
;; http://stackoverflow.com/a/31909140

(defn permutations
  [xs]
  (if (empty? xs)
    '()
    (if (= 1 (count xs))
      (list (seq xs))
      (for [x xs
            y (permutations (disj xs x))]
        (cons x y)))))

(defn abs
  [x y]
  (let [diff (- x y)]
    (if (neg? diff) (- diff) diff)))

;; can't use sets 
;; "hares" "shares" => true (should be)
(defn one-letter-diff-sets
  "Return true with the difference between a and b is only 1 character"
  [a b]
  (let [a (set a)
        b (set b)]
    (if (= 1 (count (clojure.set/difference a b)))  
      true
      ;; check if one sets are the same except one has an additional character
      (if (or (clojure.set/subset? a b)
              (clojure.set/subset? b a))
        (do 
          (println "subset")
          (= 1 (abs (count a) (count b))))
        false))))

;; clojure.data/diff version (not working)
(defn one-letter-diff
  [a b]
  (let [a (sort a)
        b (sort b)
        am (frequencies a)
        bm (frequencies b)
        diff (clojure.data/diff am bm)]
    (if (and (nil? (first diff)) (nil? (second diff)))
      (do
        (println "same")
        true)
      (if (or (nil? (first diff)) (nil? (second diff)))
        (do
          (let [kv (if (nil? (second diff)) (first (first diff)) (first (second diff)))]
            
            (println (type kv))
            (println (val kv))
            (if (val kv)
              (println "additional character in one")
              (println "multiple characters")
              )
            )
          true)
        )
      )
    diff
    ))

    ;; same

    ;; (println am bm)
    ;; (println x y z)
    ;; ;; both nil?
    ;; (if (and (nil? x) (nil? y))
    ;;   (println "both are the same")
    ;;   )
    ;; ;; both not nil?
    ;; (if (and x y)
    ;;   (do 
    ;;     (println "both not nil")
    ;;     (println (type x))
    ;;     (println (key x))
    ;;     (println (val x))
    ;;     (println (key y))
    ;;     (println (val y)))
    ;;   ;; (if (and (= (key x) (key y)) (= 1 (abs (- (val x) (val y)))))
    ;;   ;;   (println ("one letter difference"))
    ;;   ;;   )
    ;;   )
    ;; ;; one is nil?
    ;; (let [v (if (nil? x) y x)]
    ;;   (if (= 1 (val v))
    ;;     (println "One new letter")
    ;;     (println "More than one new letter")
    ;;     )
    ;;   )
    ;; [x y z]
    ;; ;; (cond
    ;; ;;    (and (nil? x) (nil? y)) true
    ;; ;;    (or (nil? x) (nil? y)) true
    ;; ;;    (and (= (key x) (key y)) (= 1 (abs (- (val x) (val y))))) true
    ;; ;;   :else false)


;; string comparison version (not working)
(defn one-letter-diff
  [a b]
  ;; sort both
  (let [a (sort a)
        b (sort b)
        lena (count a)
        lenb (count b)
        diff (abs lena lenb)]
    (if (= a b) true
        ;; same length

        ;; diff by 1 length
        (if (= 1 diff)
          (let [smaller (if (< (count a) (count b)) a b)
                larger  (if (> (count a) (count b)) a b)]
            (println "smaller", smaller)
            (println "larger", larger)
            (if (= 1 (- (count larger) (count smaller)))
              (println "one character diff")
              false)
            )
          )
        )
    )
  )

;; http://rosettacode.org/wiki/Levenshtein_distance#Clojure
(defn levenshtein [str1 str2]
  (let [len1 (count str1)
        len2 (count str2)]
    (cond (zero? len1) len2
          (zero? len2) len1
          :else
          (let [cost (if (= (first str1) (first str2)) 0 1)]
            (min (inc (levenshtein (rest str1) str2))
                 (inc (levenshtein str1 (rest str2)))
                 (+ cost
                    (levenshtein (rest str1) (rest str2))))))))

(defn one-letter-diff
  [a b]
  (< (levenshtein a b) 2))

(defn all-true 
  "Return true if xs ia ll true"
  [xs]
  (every? true? xs))

(defn prob82
  ;; build all the permutations of the set xs
  ;; map diff-by-one-letter over all the combinations
  ;; find if there is one set that is all true
  [xs]
  (let [s (permutations xs)]
    (if (some true? (map all-true (map #(map one-letter-diff % (rest %)) s)))
      true
      false)))

(defn prob82
  ;; build all the permutations of the set or words xs
  ;; levenshetien returns the number edits diff between two strings
  ;; map diff-by-one-letter over all the combinations
  ;; find if there is one set that is all true
  [xs]
  (letfn [(permutations [xs]
            (if (empty? xs)
              '()
              (if (= 1 (count xs))
                (list (seq xs))
                (for [x xs
                      y (permutations (disj xs x))]
                  (cons x y)))))
          (all-true [xs]  ;; return true xs contains all true
            (every? true? xs))
          (levenshtein [str1 str2]  ;; http://rosettacode.org/wiki/Levenshtein_distance#Clojure
            (let [len1 (count str1)
                  len2 (count str2)]
              (cond (zero? len1) len2
                    (zero? len2) len1
                    :else
                    (let [cost (if (= (first str1) (first str2)) 0 1)]
                      (min (inc (levenshtein (rest str1) str2))
                           (inc (levenshtein str1 (rest str2)))
                           (+ cost
                              (levenshtein (rest str1) (rest str2))))))))
          (one-letter-diff [a b]     ;; same string or one letter difference
            (< (levenshtein a b) 2))
          ]
    (let [s (permutations xs)]
      (if (some true? (map all-true (map #(map one-letter-diff % (rest %)) s)))
        true
        false))))


;; (= true (prob82 #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))
;; (= false (prob82 #{"cot" "hot" "bat" "fat"}))
;; (= false (prob82 #{"to" "top" "stop" "tops" "toss"}))
;; (= true (prob82 #{"spout" "do" "pot" "pout" "spot" "dot"}))
;; (= true (prob82 #{"share" "hares" "shares" "hare" "are"}))
;; (= false (prob82 #{"share" "hares" "hare" "are"}))





;; TODO #89
;; http://www.4clojure.com/problem/89


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


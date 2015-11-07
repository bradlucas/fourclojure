(ns fourclojure.hard.hard.problem-082
  (:require clojure.data))


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




 


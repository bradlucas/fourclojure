(ns fourclojure.easy)

;; ------------------------------------------------------------------------
;; Last Element
;; Penultimate Element
;; Nth Element
;; Count a Sequence
;; Sum It All Up
;; Find the odd numbers
;; Reverse a Sequence
;; Palindrome Detector
;; Fibonacci Sequence
;; Maximum value
;; Get the Caps
;; Duplicate a Sequence
;; Intro to some
;; Implement range
;; Flatten a Sequence
;; Interleave Two Seqs
;; Factorial Fun
;; Compress a Sequence
;; Replicate a Sequence
;; Contain Yourself
;; Intro to Iterate
;; Interpose a Seq
;; Pack a Sequence
;; Drop Every Nth Item
;; Split a sequence
;; Advanced Destructuring
;; A Half-Truth
;; Map Construction
;; Greatest Common Divisor
;; Comparisons
;; Set Intersection
;; Re-implement Iterate
;; Simple closures
;; Product Digits
;; Cartesian Product
;; Group a Sequence
;; Symmetric Difference
;; Read a binary number
;; dot product
;; Through the Looking Class
;; Infix Calculator
;; Indexing Sequences
;; Pascal's Triangle
;; Re-implement Map
;; To Tree, or not to Tree
;; Sum of square of digits
;; Recognize Playing Cards
;; Least Common Multiple
;; Intro to Destructuring 2
;; Pascal's Trapezoid
;; Beauty is Symmetry
;; Trees into tables
;; Pairwise Disjoint Sets
;; ------------------------------------------------------------------------
;; ------------------------------------------------------------------------


;; Last Element	
;; https://www.4clojure.com/problem/19
(fn [x] (first (reverse x)))

;; Penultimate Element	
;; https://www.4clojure.com/problem/20
(fn [x] (second (reverse x)))

;; Nth Element	
;; https://www.4clojure.com/problem/21
(fn [col n]
  (if (zero? n)
    (first col)
    (recur (rest col) (- n 1))))

;; Count a Sequence	
;; https://www.4clojure.com/problem/22
(fn [col]
  (reduce + (map (fn [x] 1) col)))

;; Sum It All Up	
;; https://www.4clojure.com/problem/24
#(reduce + %)

;; Find the odd numbers	
;; https://www.4clojure.com/problem/25
#(filter odd? %)

;; Reverse a Sequence	
;; https://www.4clojure.com/problem/23
(fn [s]
  (loop [r (rest s)
         acc (conj () (first s))]
    (if (empty? r)
      acc
      (recur (rest r) (conj acc (first r))))))

;; Palindrome Detector	
;; https://www.4clojure.com/problem/27
(defn prob27 [col] 
  (let [s (seq col)]
    (= s (reverse col))))

;; Fibonacci Sequence	
;; https://www.4clojure.com/problem/26
(fn
  [num]
  (loop [lst [1 1]]
    (if (< (count lst) num)
      (recur (conj lst (+  (last lst) (nth lst (- (count lst) 2)))))
      lst)))

;; Maximum value	
;; https://www.4clojure.com/problem/38
(defn mx [& args]
  (if (= 1 (count args))
    (first args)
    (if (> (first args) (second args))
      (apply mx (conj (rest (rest args)) (first args)))
      (apply  mx (rest args)))))

;; Get the Caps
;; https://www.4clojure.com/problem/29
;; A == 65 Z = 90
(defn prob29 [col]
  (letfn [(capital [x]
            (let [v (int x)]
              (and (> v 64) (< v 91)))
            )]
    (apply str (filter #(capital %) (seq col)))))

;; Duplicate a Sequence	
;; https://www.4clojure.com/problem/32
(defn dup [s]
  (loop [r (rest s)
         acc (conj () (first s) (first s))]
    (if (empty? r)
      (reverse acc)
      (recur (rest r) (conj acc (first r) (first r))))))

;; Intro to some	
;; https://www.4clojure.com/problem/48
6

;; Implement range	
;; https://www.4clojure.com/problem/34
;; return rnage of integer from x to y inclusive
(defn prob34 [x y]
  (take (- y x) (iterate inc x)))

;; Flatten a Sequence	
;; https://www.4clojure.com/problem/28
(defn flat [coll]
  (when-let [s (seq coll)]
    (if (coll? (first s))
      (concat (flat (first s)) (flat (rest s)))
      (cons (first s) (flatten (rest s))))))

;; Interleave Two Seqs
;; https://www.4clojure.com/problem/39
(defn prob39 [c1 c2]
  (let [s1 (seq c1) s2 (seq c2)]
    (when (and s1 s2)
      (cons (first s1) (cons (first s2) (prob39 (rest s1) (rest s2)))))))

;; Factorial Fun	
;; https://www.4clojure.com/problem/42
(defn prob42 [x]
  (reduce * (range 1 (+ 1 x))))

;; Compress a Sequence	
;; https://www.4clojure.com/problem/30
(defn prob30 [col]
  (loop [col col
         acc []]
    (if (empty? col)
      acc
      (recur
       (rest col)
       (if (not (= (last acc) (first col)))
         (conj acc (first col))
         acc
         )))))

;; Replicate a Sequence	
;; https://www.4clojure.com/problem/33
;; (dup [1 2 3] 2) => (1 1 2 2 3 3)
(defn dupx [s num]
  (letfn [(duper [n v]
            (repeat n v))]
    (loop [r (rest s)
           acc (into () (duper num (first s)))]
      (if (empty? r)
        (reverse acc)
        (recur (rest r) (into acc (duper num (first r))))))))

;; Contain Yourself	
;; https://www.4clojure.com/problem/47
4

;; Intro to Iterate	
;; https://www.4clojure.com/problem/45
'(1 4 7 10 13)

;; Interpose a Seq	
;; https://www.4clojure.com/problem/40
(defn prob40 [n col]
  (loop [n n
         col col
         acc []]
    (if (= (count col) 1)
      (conj acc (first col))
      (recur n (rest col) (conj acc (first col) n)))))

;; Pack a Sequence	
;; https://www.4clojure.com/problem/31
(defn prob31 [col]
  (partition-by identity col))

;; Drop Every Nth Item	
;; https://www.4clojure.com/problem/41
(fn [col n]
  (flatten
      (concat
    (map #(drop-last %) (partition n col))
    (take-last (rem (count col) n) col)
    )))

;; Split a sequence	
;; https://www.4clojure.com/problem/49
(defn prob49 [n col]
  [(take n col) (drop n col)]
)

;; Advanced Destructuring	
;; https://www.4clojure.com/problem/51
(= [1 2 [3 4 5] [1 2 3 4 5]] 
   (let [[a b & c :as d] 
         [1 2 3 4 5]] 
     [a b c d]))

;; A Half-Truth	
;; https://www.4clojure.com/problem/83
(defn prob83 [& args]
  (let [true-count (count (filter true? args))
        length (count args)]
    (and (> true-count 0) (< true-count length))))

;; Map Construction	
;; https://www.4clojure.com/problem/61
;; Write a function which takes a vector of keys and a vector of values and constructs a map from
;; them.
(defn prob61
  [keys values]
  (into {} (map #(hash-map %1 %2) keys values)))

;; Greatest Common Divisor	
;; https://www.4clojure.com/problem/66
;; return  the largest number that divides without remainder into both numbers
;; Euclid's algorithm (see https://en.wikipedia.org/wiki/Greatest_common_divisor)
(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (mod a b))))

(defn gcd [a b]
  (if (zero? b) 
    a
    (recur b (mod a b))))

;; Comparisons	
;; https://www.4clojure.com/problem/166
(defn prob166 [f x y]
  (cond
   (f x y) :lt
   (f y x) :gt
   :else :eq))
;;(prob166 (fn [x y] (< (count x) (count y))) "pear" "plum")

;; Set Intersection	
;; https://www.4clojure.com/problem/81
(defn prob81 [set1 set2]
  ;; for each element in s1 if in s2 return in result
  ;; should start with largest set (or same set size)
  (loop [s1 set1
         s2 set2         
         res []
         ]
    (if (empty? s1)
      (set (reverse res))
      (let [item (first s1)]
        (if (contains? s2 item)
          (recur (disj s1 item) s2 (conj res item))
          (recur (disj s1 item) s2 res))))))

;; Re-implement Iterate	
;; https://www.4clojure.com/problem/62
(defn prob62 
  [f x]
  (lazy-seq (cons x (prob62 f (f x)))))

;; Simple closures	
;; https://www.4clojure.com/problem/107
(defn prob107 [n]
  (fn [x] (reduce * (repeat n x)))
  )

;; Product Digits	
;; https://www.4clojure.com/problem/99
(defn prob99 [x y]
  (vec (map #(- (int %) 48) (seq (str (* x y))))))

;; Cartesian Product	
;; https://www.4clojure.com/problem/90
(defn prob90 [l1 l2]
  (loop [acc #{}
         rows l1
         cols l2
         ]
    (if (empty? rows)
      acc
      (let [r (first rows)]
        (recur (into acc (map (fn [v] [r v]) cols)) (rest rows) cols)
        ))))

;; Group a Sequence	
;; https://www.4clojure.com/problem/63
(defn prob63 
  [f col]
  (reduce
   (fn [ret x]
     (let [key (f x)]
       (assoc ret key (conj (get ret key []) x)))
     )
   {} col))

;; Symmetric Difference	
;; https://www.4clojure.com/problem/88
#(set (reduce conj 
          (remove % %2) 
          (remove %2 %)))

;; Read a binary number	
;; https://www.4clojure.com/problem/122
#(Integer/parseInt % 2)

;; dot product	
;; https://www.4clojure.com/problem/143
(defn prob143 [v1 v2]
  (reduce + (map #(* %1 %2) v1 v2))
)

;; Through the Looking Class	
;; https://www.4clojure.com/problem/126
(let [x Class]
  (and (= (class x) x) x))

;; Infix Calculator	
;; https://www.4clojure.com/problem/135
(defn prob135 [val & others]
  ;; a X b X c
  (loop [acc val
         stack others]
    (if (empty? stack)
      acc
      (let [op (first stack)
            b (second stack)
            rem (rest (rest stack))]
        (recur (op acc b) rem)))))

;; Indexing Sequences	
;; https://www.4clojure.com/problem/157
(defn prob157 [col]
  (loop [col col
         idx 0
         ret []]
    (if (empty? col)
      ret
      (recur (rest col) (inc idx) (conj ret [(first col) idx])))))

;; Pascal's Triangle	
;; https://www.4clojure.com/problem/97
(defn prob97 [row]
  (last (take row
        (iterate #(concat [1]
                          (map + % (rest %))
                          [1])
                 [1]))))

;; Re-implement Map	
;; https://www.4clojure.com/problem/118
(defn prob118 [f col]
  ;; return a lazy seq with f applied to each item in col
  (if (empty? col)
    nil
    (lazy-seq (cons (f (first col)) (prob118 f (rest col))))))

(defn prob118 [f col]
  ;; return a lazy seq with f applied to each item in col
  ;; This one isn't working....
  (loop [f f
         col col
         acc []]
    (if (empty? col)
      acc
      (recur f (rest col) (conj acc (f (first col)))))))

;; To Tree, or not to Tree	
;; https://www.4clojure.com/problem/95
(defn prob95 [s]
  (if (nil? s)
    true
    (if (or (not (coll? s)) (not= 3 (count s)))
      false
       (and (prob95 (nth s 1)) (prob95 (nth s 2))))))
;; '(:a (:b nil nil) nil)

;; Sum of square of digits	
;; Write a function which takes a collection of integers as an
;; argument. Return the count of how many elements are smaller than
;; the sum of their squared component digits. For example: 10 is
;; larger than 1 squared plus 0 squared; whereas 15 is smaller than 1
;; squared plus 5 squared.
;; #120
;; https://www.4clojure.com/problem/120
(defn count-less-sum-square-digits [coll]
  (letfn [
          (sum-square-num-range [col]
            (reduce + (map #(square-num %) col)))
          (square-num [n]
            (* n n ))
          (digit-list [num]
            (map char-to-num (str num)))
          (char-to-num [c]
            (- (int c) 48))
          ]
  (count (filter  #(true? %) (map #(< % (sum-square-num-range (digit-list %))) coll)))))

;; Recognize Playing Cards	
;; https://www.4clojure.com/problem/128
(defn prob128 [s]
    (letfn [(suit [c]
              ({
                \D :diamond
                \H :heart
                \C :club
                \S :spade
                } 
               c)
              )
            (rank [c]
              ({
                \2 0
                \3 1
                \4 2
                \5 3
                \6 4
                \7 5
                \8 6
                \9 7
                \T 8
                \J 9
                \Q 10
                \K 11
                \A 12
                }
               c
               )
              )]
      (let [v (vec s)
            suit-char (first v)
            rank-char (last v)]
        {:suit (suit suit-char) :rank (rank rank-char)})))

;; Least Common Multiple	
;; https://www.4clojure.com/problem/100
;; smallest number dividable by each of the args
;; lcm = (/ (* a b) (gcd a b)
;; gcd(a, 0) == a
;; gcd(a, b) == gcd(b, (mod a b))
(defn prob100 [& args]
  (reduce 
   (fn [x y]
     (letfn [(gcd [a b]
               (if (zero? b) 
                 a
                 (recur b (mod a b))))
             ]
       (/ (* x y) (gcd x y)))
     )
   args))

;; Intro to Destructuring 2	
;; https://www.4clojure.com/problem/173
(= 3
  (let [[f s] [+ (range 3)]] (apply f s))
  (let [[[f s] b] [[+ 1] 2]] (f s b))
  (let [[f s] [inc 2]] (f s)))



;; Pascal's Trapezoid	
;; https://www.4clojure.com/problem/147
(defn prob147 [row]
  (iterate #(concat [(first %)]
                    (map +' % (rest %))
                    [(last %)])
                 row))



;; Beauty is Symmetry	
;; https://www.4clojure.com/problem/96
(defn prob96 [[_ l r]]
  (if (not (coll? l))
    true
    (if (not (and (coll? l) (coll? r)))
      false
      (let [[lv ll lr] l
            [rv rl rr] r]
        (and (= lv rv) (prob96 [nil ll rr]) (prob96 [nil rl lr]))
))))

(map prob96 [
             '(:a (:b nil nil) (:b nil nil))
             '(:a (:b nil nil) nil)
             '(:a (:b nil nil) (:c nil nil))
             [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
              [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]]
             [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
              [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]]
             [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
              [2 [3 nil [4 [6 nil nil] nil]] nil]]
             ])

;; Trees into tables	
;; https://www.4clojure.com/problem/146
(
 (fn prob146 [t]
   (apply merge   (for [[k v] t
         [k2 v2] v]
     {[k k2] v2}
     ))
   )
'{a {p 1, q 2}
  b {m 3, n 4}}
)

;; Pairwise Disjoint Sets
;; https://www.4clojure.com/problem/153	
(defn prob153 [s]
  (let [ss (seq s)
        length (count ss)]
    (every? empty? (for [s1 (range length)
                         s2 (range length)
                         :when (not= s1 s2)]
                     (clojure.set/intersection (nth ss s1) (nth ss s2))))))

;; #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}}

;; ----------------------------------------------------------------------------------------------------
;; EOF
;; ----------------------------------------------------------------------------------------------------


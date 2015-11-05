(ns fourclojure.medium.medium)

;; Intro to Destructuring 2
;; Pascal's Trapezoid
;; Beauty is Symmetry
;; Trees into tables
;; Pairwise Disjoint Sets
;; Flipping out
;; Rotate Sequence
;; Reverse Interleave
;; Split by Type
;; Count Occurrences
;; Find Distinct Items
;; Function Composition
;; Partition a Sequence
;; Juxtaposition
;; Word Sorting
;; Prime Numbers
;; Filter Perfect Squares
;; Black Box Testing
;; Intro to Trampoline
;; Perfect Numbers
;; Anagram Finder
;; Sequence Reductions
;; Merge with a Function
;; intoCamelCase
;; Euler's Totient Function
;; Happy numbers
;; Reimplement Trampoline
;; The Balance of N
;; Power Set
;; Equivalence Classes
;; Identify keys and values
;; Digits and bases
;; Sequence of pronunciations
;; Oscilrate
;; Lazy Searching
;; Decurry
;; Partially Flatten a Sequence
;; Global take-while
;; Insert between two items
;; Write Roman Numerals
;; Generating k-combinations
;; Prime Sandwich
;; Universal Computation Engine
;; The Big Divide
;; Intervals
;; Sum Some Set Subsets
;; Balancing Brackets
;; Sequs Horribilis
;; Tricky card games
;; Palindromic Numbers
;; Infinite Matrix
;; Parentheses... Again


;; Flipping out
;; https://www.4clojure.com/problem/46
;; return functino that flips the order of the aruments of the input function
(defn prob46 [f]
  (fn [x y]
    (f y x)))

;; Rotate Sequence #44
;; https://www.4clojure.com/problem/44
(defn prob44 [n s]
  (let [len (count s)
        idx (cond 
             (< n 0) (+ len (mod n (- len)))
             (> n len) (mod n len)
             :else n)]
    (lazy-cat (drop idx s) (take idx s))))


;; (= (prob44 2 [1 2 3 4 5]) '(3 4 5 1 2))
;; (= (prob44 -2 [1 2 3 4 5]) '(4 5 1 2 3))
;; (= (prob44 6 [1 2 3 4 5]) '(2 3 4 5 1))
;; (= (prob44 1 '(:a :b :c)) '(:b :c :a))
;; (= (prob44 -4 '(:a :b :c)) '(:c :a :b))




;; Reverse Interleave
;; 43
(defn prob43 [col n]
  (apply map list (partition n col)))


(= (prob43 [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
(= (prob43 (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
(= (prob43 (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))



;; Split by type
;; 50
(defn prob50 [col]
  (map second (group-by type col)))

(= (set (prob50 [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
(= (set (prob50 [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
(= (set (prob50 [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})



;; Count Occurrences
;; #55
(defn prob55 [col]
  (loop[ret {}
        col col]
    (if (empty? col)
      ret
      (recur 
       (assoc ret (first col) (inc (get ret (first col) 0)))
       (rest col)))))



;; Find Distinct Items
;; #56
(defn prob56 [s]
  (loop [acc []
         col s]
    (if (empty? col)
      acc
      (recur 
       (if (not (some #(= (first col) %) acc))
         (conj acc (first col))
         acc
         )
       (rest col)))))




;; Function Composition 
;; #58
;; apply params functions from right to left
(defn prob58 [& funcs]
  (fn [& args]
    (first 
     (reduce 
      #(vector (apply %2 %1))
      args 
      (reverse funcs)))
    ))

;; (defn prob58 [& funcs]
;;   (fn [& args]
;;     (let [[f & fs] (reverse funcs)]
;;       (reduce #(%2 %1) (apply f args) fs))))



;; Partition a Sequence
;; #54
(defn prob54 [n s]
  (loop [n n
         s s
         acc ()]
    (if (< (count s) n)
      (reverse  acc)
      (recur n (drop n s) (conj acc (take n s))))))


(defn prob54 [n s]
  (loop [col s
         acc ()]
    (if (< (count col) n)
      (reverse  acc)
      (recur (drop n col) (conj acc (take n col))))))




;; Juxtaposition
;; 59
(defn prob59 [& funcs]
  (fn [& args]
    (map #(apply % args) funcs)
    ))



;; Word Sorting
;; #70
(defn prob70 [s]
  (let [cleaned (apply str (filter #(or (Character/isLetter %) (Character/isSpace %)(Character/isDigit %)) s))
        words (clojure.string/split cleaned #"\s")
        ]
    (sort #(.compareTo 
            (clojure.string/lower-case (read-string %1)) 
            (clojure.string/lower-case ( read-string %2))) words)))



;; Primes
;; 67
(defn prob67 [n]
  (take n
        (cons 2 (filter 
                 (fn [x] ;; is prime
                   (empty? (filter #(= 0 (mod x %)) (range 2 x)))
                   )
                 (iterate #(+ 2 %) 3)))))



;; Filter Prefect Squares
;; 74
;; separate on commas into ints
;; filter perfect square
;; return comma separated list
(defn prob74 [comma-string]
  (let [string (clojure.string/split comma-string #",")
        nums (map read-string string)
        ]
    (apply str (interpose "," (filter #(let [x (Math/sqrt %)] (= (float (int x)) x))  nums)))))

;; perfect square
;; square too to x.0
;; if you truncate to an int and go back to float and still have  the same number you have x.0





;; Black Box Testing
;; 65

;; Figure out the input by testing it. Return :map, :set, :list, or :vector 
(defn prob65
  [col]
  (let [blank (empty col)]
    (cond
     (= blank {}) :map
     (= blank #{}) :set
     (= blank '()) (if (reversible? blank):vector :list)
     :else :unknown)))





;; Intro to Trampoline
;; 76
(= [1 3 5 7 9 11]
   (letfn
       [(foo [x y] #(bar (conj x y) y))
        (bar [x y] (if (> (last x) 10)
                     x
                     #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))



;; Perfect Numbers
;; #80
;; a number is perfect if the sum of its divisors equal the number itself
(defn prob80
  [num]
  (letfn [(divisors [n]
            (filter #(zero? (mod n %)) (range 1 (+ 1 (/ n 2))))
            )]
    (= (reduce + (divisors num)) num)))




;; Anagram Finder
;; 77

;; (def words ["meat" "mat" "team" "mate" "eat"])
(defn prob77
  [words]

  ;; group-by sort words
  (group-by sort words)
  ;; {(\a \e \m \t) ["meat" "team" "mate"], (\a \m \t) ["mat"], (\a \e \t) ["eat"]}

  ;; pull out the values
  (map val (group-by sort words))
  ;; (["meat" "team" "mate"] ["mat"] ["eat"])

  ;; filter out the ones with at least two words
  (filter #(> (count %) 1) (map val (group-by sort words)))

  ;; put results into a set
  (into #{} (filter #(> (count %) 1) (map val (group-by sort words))))
)

(defn prob77
  [words]
  (into #{} (map set (filter #(> (count %) 1) (map val (group-by sort words))))))




;; Squence Reductions
;; #60
(defn prob60
  ([f coll] 
   (prob60 f (first coll) (rest coll)))
  ([f v coll] 
   (if (seq coll)
     (let [next-v (f v (first coll))
           next-coll (rest coll)
           ]
       (cons v (lazy-seq (prob60 f next-v next-coll)))
       )
     (cons v (lazy-seq '())))))



;; Merge with a Function
;; 69
(defn prob69 [f & maps]
  (reduce
   (fn [m1 maps]
     (reduce
      (fn [map [key val]]
        (println map key val)
        (assoc map key (if (maps key) (f val (maps key)) val))
        )
      maps m1
      ))
   (first maps) (rest maps)))


(= (prob69 * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   {:a 4, :b 6, :c 20})




;; intoCamelCase
;; 102

;; split at hyphens and captialize subsequent words
(defn prob102
  [string]
  (let [words (clojure.string/split string #"-")
        num (count words)
        ]
    (if (= 1 num)
      (first words)
      (apply str (concat  [(first words)] (vec (map clojure.string/capitalize (rest words))))))))



;; Euler's Totient Function
;; #75
;; Two numbers are coprime if their greatest common divisor equals 1.
;; Euler's totient function f(x) is defined as the number of positive integers 
;; less than x which are coprime to x.
(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (mod a b))))

(defn coprime [a b]
  (= 1 (gcd a b)))

;; number of positive integers less than x
(defn prob75
  [x]
  (cond
   (= x 1) 1
   :else (count (filter #(coprime % x) (range x)))))

(defn prob75
  [x]
  (cond
   (= x 1) 1
   :else (letfn [(gcd [a b] 
                   (if (zero? b)
                     a
                     (gcd b (mod a b))))
                 (coprime [a b]
                   (= 1 (gcd a b)))
                 ]
           (count (filter #(coprime % x) (range x))))))




;; Happy numbers
;; #86
;; - Take each individual digit
;; - square
;; - sum the squares
;; - repeat with the new number
;; - repeat until the numer is 1 upon which you've determned you have a happy number
;; - if the process does not end in 1 then the number is unhappy
(defn prob86
  ([num] (prob86 num #{}))
  ([num seen]
   (letfn [(nextnum [num]
             (reduce + (map #( * % %) (digit-list num))))
           
           (digit-list [num]
             (map char-to-num (str num)))
           
           (char-to-num [c]
             (- (int c) 48))
           ]
     (if (= 1 num) true
         (if (contains? seen num) false
             (prob86 (nextnum num) (conj seen num)))))))


;; 
(defn digits [num]
  (map #(Character/digit % 10) (str num)))




(defn get-nth-pos-of-n
  [n]
  ;; in the sequence of primes, what position is n
  ;; recur of the list of primes until you find n or you go past it (n is not a prime)
  (loop [pos 1]
    (print pos)
    (let [v (last (prob67 pos))]
    (if (= v n)
      pos
      (if (> v n)
        false
        (recur (+ pos 1)))))))

(defn meanmiddle
  "Return true if b is the mean of a and c"
  [a b c]
  (= b (/ (+ a c) 2)))

(defn get-nth-prime
  [pos]
  ;; return the nth prime
  (last (prob67 pos)))

(defn prob116 
  [n]
  (let [pos (get-nth-pos-of-n n)]
    (if pos
      (meanmiddle (get-nth-prime (- pos 1)) n (get-nth-prime (+ pos 1))) 
      false)))

(defn prob116 
  [n]
  (letfn [(prob67 [n]
            (take n
                  (cons 2 (filter 
                           (fn [x] ;; is prime
                             (empty? (filter #(= 0 (mod x %)) (range 2 x)))
                             )
                           (iterate #(+ 2 %) 3)))))
          (get-nth-pos-of-n [n]
            ;; in the sequence of primes, what position is n
            ;; recur of the list of primes until you find n or you go past it (n is not a prime)
            (loop [pos 1]
              (print pos)
              (let [v (last (prob67 pos))]
                (if (= v n)
                  pos
                  (if (> v n)
                    false
                    (recur (+ pos 1)))))))
          (get-nth-prime [pos]
            ;; return the nth prime
            (last (prob67 pos)))
          (meanmiddle [a b c]
            (= b (/ (+ a c) 2)))
          ]
    (let [pos (get-nth-pos-of-n n)]
      (if pos
        (meanmiddle (get-nth-prime (- pos 1)) n (get-nth-prime (+ pos 1))) 
        false))))

(defn prob116
  [n]
  (let [prime? (fn [x]
                 (if (or (= x 1) (= x 2))
                   true
                   (empty? (filter #(= 0 (mod x %)) (range 2 x)))))
        primes (filter prime? (drop 1 (range)))
        meanmiddle (fn [a b c] (= b (/ (+ a c) 2)))]
    (if (or (not (prime? n)) (<= n 2))
      false
      (let [prev (last (take-while #(< % n) primes))
            next (first (drop-while #(<= % n) primes))]
        (meanmiddle prev n next)))))






;; Idenity keys and values
;; #105
(defn prob105 [s]
  (loop [s s
         prev nil
         ret {}]
    (if (empty? s)
      ret
      (let [f (first s)
            key (if (keyword? f) f prev)
            prev key
            val (ret key nil)
            ]
        (recur (rest s) key (assoc ret key (if (keyword? f) [] (conj (ret key) f))))
        ))))


;; TODO
;; #137
;;
;; Write a function which returns a sequence of digits of a non-negative
;; number (first argument) in numerical system with an arbitrary base
;; (second argument). Digits should be represented with their integer
;; values, e.g. 15 would be [1 5] in base 10, [1 1 1 1] in base 2 and
;; [15] in base 16.

;; 15 = 1 * 10^1 + 5 * 10^0
;; 15 = (1 * 2^3) + (1 * 2^2) + (1 * 2^1) + (1 * 2^0)

(defn prob137
  [num base]
  [1 5]
)

(= (prob137 15 10) [1 5])
(= (prob137 15 2) [1 1 1 1])

(defn ** [x n] (reduce * (repeat n x)))




;; Sum Some Set Subsets
;; # 131
;; 
;; Given a variable number of sets of integers, create a function which
;; returns true iff all of the sets have a non-empty subset with an
;; equivalent summation.


;; http://stackoverflow.com/a/20613311
(defn subsets [s]
  (if (seq s)
    (let [f (first s), srs (subsets (disj s f))]
      (concat srs (map #(conj % f) srs)))
    (list #{})))

(defn add-contents
  "For a given set return the result of summing it's contents"
  [s]
  (reduce + s))

(defn add-set-members
  "For sequences of sets add each's contents and add the results to a set"
  [xs]
  (into #{} (map add-contents xs)))

(defn prob131
  [& sets]
  ;; find all the subsets in each set and sum the values
  ;; find the intersection of all the subset sums
  ;; if there is a common sum then return true
  ;;(map #(map #(reduce + (map identity %)) (vec (subsets %))) sets)
  (pos? (count (apply clojure.set/intersection (map #(add-set-members %) (map #(filter not-empty ( subsets %)) sets))))))



(= true (prob131 #{-1 1 99} 
                 #{-2 2 888}
                 #{-3 3 7777})) ; ex. all sets have a subset which sums to zero


(= false (prob131 #{1}
                  #{2}
                  #{3}
                  #{4}))



;; http://stackoverflow.com/a/20613311
(defn subsets [s]
  (if (seq s)
    (let [f (first s), srs (subsets (disj s f))]
      (concat srs (map #(conj % f) srs)))
    (list #{})))

(defn add-contents
  "For a given set return the result of summing it's contents"
  [s]
  (reduce + s))

(defn add-set-members
  "For sequences of sets add each's contents and add the results to a set"
  [xs]
  (into #{} (map add-contents xs)))


;; 09-28-2015 Submitted
(defn prob131
  [& sets]
  (letfn [(subsets [s]
            (if (seq s)
              (let [f (first s), srs (subsets (disj s f))]
                (concat srs (map #(conj % f) srs)))
              (list #{})))
          (add-contents [s]
            (reduce + s))
          (add-set-members [xs]
            (into #{} (map add-contents xs)))]
    (pos? (count (apply clojure.set/intersection (map #(add-set-members %) (map #(filter not-empty ( subsets %)) sets))))))
  )



;; 171
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

;; 115
;; Balanced Number
;; 121 => true
;; 123 => false
;; 0-9 => true

(defn digits [num]
  (map #(Character/digit % 10) (str num)))

(defn split-middle 
  [xs]
  (let [len (count xs)
        middle (int (/ len 2))
        left (take middle xs)
        middle2 (if (odd? len) (inc middle) middle)
        right (drop middle2 xs)
        ]
    (= left right)))


(defn prob115 
  [n]
  (if (< n 10) true
      (letfn [(digits 
                [num] 
                (map #(Character/digit % 10) (str num)))
              (split-middle [xs]
                (let [len (count xs)
                      mid (int (/ len 2))
                      left (take mid xs)
                      mid (if (odd? len) (inc mid) mid)
                      right (drop mid xs)
                      ]
                  (= (reduce + left) (reduce + right))))
              ]
        (split-middle (digits n)))))



;; 104
;; http://www.numericana.com/answer/roman.htm#valid
;; I=1, V=5, X=10, L=50, C=100, D=500, M=1000


;; (= [2 3 5 7 11 13]
;;   (__ 4 #(= 2 (mod % 3))
;;          [2 3 5 7 11 13 17 19 23]))

(defn prob114
  [n f xs]
  ;; loop over xs and count the number of times f[x] is true
  ;; top when you've counted n times
  (loop [cnt 0
         acc []
         xs xs]
    (if (= cnt n) 
      (butlast acc)
      (recur (if (f (first xs)) (inc cnt) cnt) (conj acc (first xs)) (rest xs)))))


;; 144

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



;; 78
(defn prob78 
  [f & args]
  (if (empty? args)
    (let [ret (f)]
      (if (fn? ret)
        (prob78 ret)
        ret))
    (prob78 #(apply f args))))

(defn foo []
  (letfn [(triple [x] #(sub-two (* 3 x)))
          (sub-two [x] #(stop?(- x 2)))
          (stop? [x] (if (> x 50) x #(triple x)))]
    (prob78 triple 2)))



;; #85
(defn prob85
  [xs]
  (set (map set 
            (if (empty? xs) 
              '(())
              (clojure.set/union (prob85 (next xs))
                                 (set (map #(conj % (first xs)) (prob85 (rest xs)))))
              ))))



;; 98
(defn prob98
  [f s]
  (set (map set (vals (group-by f s)))))

(= (prob98 #(* % %) #{-2 -1 0 1 2})
   #{#{0} #{1 -1} #{2 -2}})


;; 137

;; Function convert number to base. Return vector of digits

;;  e.g. 15 would be [1 5] in base 10, [1 1 1 1] in base 2 and [15] in base 16. 


(= [1 2 3 4 5 0 1] (prob137 1234501 10))
(= [1 0 0 1] (prob137 9 2))

[1 2 8 ]

(defn exp [x n]
  (reduce * (repeat n x)))
(defn base-range [x] (map #(exp x %) (range)))
(defn base-range-values [x] (map #(* x %) (base-range x)))

;; reduce over the rest
;; (quot 323 100) => 3
;; (rem 323 100) => 23
;; (quot 23 10) => 2
;; (rem 23 10) => 3
 

(defn prob137
  [num base]
  (letfn [(exp [x n] (reduce * (repeat n x)))
          (base-range [x] (map #(exp x %) (range)))]
    (let 
        [values (reverse (take-while #(< % num) (base-range base)))]
      (loop [acc []
             values values
             num num]
        (if (empty? values)
          acc
          (recur (conj acc (quot num (first values))) (rest values) (rem num (first values))))))))



(defn prob137
  [num base]
  (loop [n num
         acc ()]
    (if (zero? n)
      (if (empty? acc) '(0) acc)
      (recur (int (/ n base)) (conj acc (mod n base))))))



;; 104
;; Write Roman Numerals

;; (= "MMMCMXCIX" (__ 3999))





;;----------------------------------------------------------------------------------------------------
;; 177
;; Balancing Brackest


;; for each open push on stack and recur with rest
;; for each close verify that top of stack has it's open remove both


;; use '() as stack so conj pushes on the top
(defn prob177 
  ([s] (prob177 s '()))
  ([s stack]
   (let [is-open? #(#{\{ \( \[} %)
         is-close? #(#{\} \) \]} %)
         matches? (fn [open close]
                    (cond 
                      (and (= open \{) (= close \})) true
                      (and (= open \() (= close \))) true
                      (and (= open \[) (= close \])) true
                      :else false))]
     (if-let [c (first s)]
       (cond
         (is-open? c) (recur (rest s) (conj stack c))
         (is-close? c) (when (matches? (first stack) c)
                         (recur (rest s) (rest stack)))
         :else (recur (rest s) stack)
         )
       (empty? stack)))))




;; 132
;; Insert between two items
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



;; 104
;; Write Roman Numerals
;;
;; Maximum number is 4000
;;
;; http://turner.faculty.swau.edu/mathematics/materialslibrary/roman/
;; http://www.onlineconversion.com/roman_numerals_advanced.htm
;;
;; I	V	X	L	C	D	M
;; 1	5	10	50	100	500	1000


;; write    nstead of	value
;; IV	      IIII	4
;; IX	      VIIII	9
;; XL	      XXXX	40
;; XC	      LXXXX	90
;; CD	      CCCC	400
;; CM	      DCCCC	900


;; smaller in front of larger means subtract the smaller from the larger
(defn prob104 [n]
  (letfn [(thousands-str [n] (apply str (take n (cycle "M"))))
          (hundreds-str [n] 
            (cond
              (< n 4) (apply str (take n (cycle "C")))
              (= n 4) "CD"
              (= n 5) "D"
              (= n 9) "CM"
              :else (apply str "D" (take (- n 5) (cycle "C")))))
          (tens-str [n] 
            (cond
              (< n 4) (apply str (take n (cycle "X")))
              (= n 4) "XL"
              (= n 5) "L"
              (= n 9) "XC"
              :else  (apply str "L" (take (- n 5) (cycle "X")))))
          (ones-str [n] 
            (cond
              (< n 4) (apply str (take n (cycle "I")))
              (= n 4) "IV"
              (= n 9) "IX"
              (= n 5) "V"
              :else  (apply str "V" (take (- n 5) (cycle "I")))))]
    (let [thousands (quot n 1000)
          hundreds (mod (quot n 100) 10)
          tens (mod (quot n 10) 10)
          ones (rem n 10)
          ]
      ;; (println thousands hundreds tens ones)
      (apply str (concat (thousands-str thousands) (hundreds-str hundreds) (tens-str tens) (ones-str ones)))
      )
    )
  )


;; 110 
;; Sequence of pronunciations

;; lazy sequence of pronunciations
;; each element being a pronunciation of the previous element


;; [1 1] => [2 1] => "two ones" => "one two, one one"

;; @see https://twitter.com/fusupo/status/593484015199485953
;; @see https://www.refheap.com/100212

(defn probl10 [col] 
  (rest 
   (iterate (fn [x] (mapcat (fn [y] [(count y) (first y)]) (partition-by identity x)))
            col)))



;; 108
;; Lazy Searching

;; (map (fn [& args] (reduce + args)) [1 2 3] [1 2 3] [1 2 3])

;; [1 2 3] [1 2 3] [1 2 3]

;; http://image.slidesharecdn.com/soligorsk-130123102755-phpapp01/95/isoligorsk-3-2013-48-638.jpg?cb=1358936997

(defn prob108
  [& colls]
  (letfn [(all-same? 
            [& args]
            (let [first-value (first args)]
              (every? (fn [x] (= first-value x)) args)))]
    (map all-same? colls)
    )
  )

;; https://github.com/khotyn/4clojure-answer/blob/master/108-lazy-searching.clj
;; https://gist.github.com/prajwalit/1186723
;; http://pastebin.com/WubAHDmV

(defn max-first-value
  [colls]
  (apply max (map first colls)))

(defn all-first-same
  [cols]
  (apply = (map first cols)))

(defn drop-leading-smaller
  "Remove all leading values in each coll where it is smaller than val"
  [val colls]
  (map (partial drop-while #(< % val)) colls)
  )

(defn prob108
  [& colls]
  (let [max-first-value (max-first-value colls)]
    (if (all-first-same colls)
      max-first-value
      (recur (drop-leading-smaller max-first-value colls)))))




(defn prob108
  [& colls]
  (letfn [(max-first-value
            [colls]
            (apply max (map first colls)))
          (all-first-same
            [cols]
            (apply = (map first cols)))
          (drop-leading-smaller
            [val colls]
            (map (partial drop-while #(< % val)) colls)
            )]
  (let [max-first-value (max-first-value colls)]
    (if (all-first-same colls)
      max-first-value
      (recur (drop-leading-smaller max-first-value colls))))
))


;; 93
;; Partially Flatten a Sequence

;; [["Do"] ["Nothing"]]
;; [[[[:a :b]]] [[:c :d]] [:e :f]] => [[:a :b] [:c :d] [:e :f]]

(defn prob93 [coll]
  (when-let [s (seq coll)]
    (if (and (coll? (first s)) (coll? (first (rest s))))
      (concat (prob93 (first s)) (prob93 (rest s)))
      (cons (first s) (flatten (rest s))))))

;; https://github.com/khotyn/4clojure-answer/blob/master/93-partially-flatten-a-sequence.clj
(defn prob93
  ([coll] (prob93 coll []))
  ([coll result]
   (if (seq coll)
     (let [first-item (first coll)]
       (prob93 (next coll)
               (if (some coll? first-item)
                 (reduce #(conj %1 %2) result (prob93 first-item []))
                 (conj result first-item))))
     result
     )
   )
  )


;; 195
;; Parentheses... Again

;; 0 => #{""}
;; 1 => #{"()"}
;; 2 => #{"()()" "(())"}
;; 3 => #{"((()))" "()()()" "()(())" "(())()" "(()())"}

;; @see https://github.com/MikaelSmith/4clojure/blob/master/195%20-%20Parentheses...%20Again.clj

(defn prob195
  ([n] (prob195 "" n 0 0))
  ([s n open close] 
   ;; when n equals close return s in set as result
   (if (= n close)
     #{s}
      (clojure.set/union
       ;; while n is greater than open
       (if (< open n)
         (prob195 (str s "(") n (inc open) close)
         #{}
         )
       ;; while open is greater than close
       (if (< close open)
         (prob195 (str s ")") n open (inc close))
         #{}
         ))
     )
   )
)


;; 103
;; Generating k-combinations

;; given a sequence of n elements
;; generate all possible sets consiting of k distinct elements taken from S

;; http://pastebin.com/YPn6hsU0
;; https://github.com/brotherb/4clojure/blob/master/103.clj

;; https://cascade.ceas.uc.edu/course/16/public/blog/post/32
;Generate all n choose k combinations from a set of n items
;by concatenating all those that include the first item
;with all those that do not.            
(defn k-comb [ k items ]
  (cond
     (= k 0) '(())
     (empty? items) '()
      :else 
          (concat 
               (map (fn [it] (cons (first items) it)) (k-comb (dec k) (rest items)))
                (k-comb k (rest items)))))


(defn prob103
  [num seq]
  (set (map set   
            (cond
              (= num 0) '(())
              (empty? seq) '()
              :else (concat 
                     (map (fn [it] (cons (first seq) it)) (prob103 (dec num) (rest seq)))
                     (prob103 num (rest seq)))))))


;; 148
;; The Big Divide

;; sum of all natural numbers under n which are dividable by a or b
;; [10 3 5] => (+ 3 5 6 9) == 23

;; a and b are coprimes;; https://en.wikipedia.org/wiki/Coprime_integers

;; ------------------------------------------------------------------------
;; https://groups.google.com/forum/#!topic/4clojure/21p8zcSjopU
(defn prob148 
  [n a b] 
  (reduce + (clojure.set/union (set (range 0 n a))
                               (set (range 0 n b)))))
;; ------------------------------------------------------------------------

;; ------------------------------------------------------------------------
;; https://github.com/khotyn/4clojure-answer/blob/master/148-the-big-divide.clj
;; ------------------------------------------------------------------------

;; ------------------------------------------------------------------------
;; https://www.reddit.com/r/Clojure/comments/2qbzai/clojure_and_project_euler/cn6p5xo
(defn big-divide [n a b]
  (letfn [(n-sum [x limit]
        (let [n-val (quot limit x)]
          (if (pos? n-val)
            (-> (inc n-val) (*' n-val) (/ 2) (*' x) bigint)
            0)))]
(let [limit (dec n)]
  (-> (+' (n-sum a limit) (n-sum b limit))
      (-'  (n-sum (*' a b) limit))))))
;; ------------------------------------------------------------------------

;; ------------------------------------------------------------------------
;; https://en.wikipedia.org/wiki/Arithmetic_progression
;; Sum of n terms of an sequence with n starting 
;; n is the number of terms
;; a is the first term
;; b is the last term
;; NOTE: you need to know the number of terms (n) and the last term
;; (sum-arithmetic-sequence 334 0 999) => 166833
;; (reduce + (range 0 1000 3)) => 166833
(defn sum-arithmetic-sequence
  [n a b]
  (* (/ n 2) (+ a b)))
;; ------------------------------------------------------------------------


(defn term-count 
  "Return the number of items in the range from 0 to bound incremented by v
  Sound equal (count (range 0 bound v))"
  [bound v] 
  (+ (quot bound v) (if (> (rem bound v) 0) 1 0)))

(defn last-term 
  "The last term an in the sequence from a1 to (n-1)d
  where n is the number of terms in the progression (term-count n v)
  (last-term (term-count n d) d)
  "
  [cnt d] 
  (* (dec cnt) d))

(defn sum-terms 
  "The sum of a artithmetic squence is n/2 * (a1 + an)
  n is the numer of terms and an is the last term with d being the interval
  (sum-terms (term-count n d))
  "
  [bound d] 
  (let [cnt (term-count bound d)
        last (last-term cnt d)
        ]
    (println "cnt" cnt)
    (println "last" last)
    (*' (/ cnt 2) last)))
      
(defn prob148 
  [n a b] 
  (if (and (> n a) (> n b))
    (let [fifteen (if (> n (* a b)) (sum-terms n (* a b)) 0)]
      (- (+ (sum-terms n a) (sum-terms n b)) fifteen)
      )
    0))




      
(defn prob148 
  [n a b] 
  (letfn [(term-count [bound v] (+ (quot bound v) (if (> (rem bound v) 0) 1 0)))
          (last-term [cnt d] (* (dec cnt) d))
          (sum-terms [bound d] (let [cnt (term-count bound d)
                                     last (last-term cnt d)]
                                 (*' (/ cnt 2) last)))]
    (if (and (> n a) (> n b))
      (let [fifteen (if (> n (* a b)) (sum-terms n (* a b)) 0)]
        (- (+ (sum-terms n a) (sum-terms n b)) fifteen))
      0)))




;; 150
;; Palindromic Numbers
 
;; http://www.4clojure.com/problem/150

;; http://stackoverflow.com/questions/19249851/4clojure-palindrome-number-timeout-issue

;; Write a function which takes an integer n, as its only argument, and returns an increasing lazy sequence of all palindromic numbers that are not less than n.

(defn palindrome? [s]
             (let [s (seq (partition 1 (str s)))]
               (= s (reverse s))))


;; faster than previous
(defn palindrome? [^String s]
  (= s (clojure.string/reverse s)))

;; https://groups.google.com/d/msg/clojure/DNdWjsc7av8/_rn0Zzkn5_EJ
 (defn palindrome?
  [n]
  (let [s (str n)
        len (.length s)
        mid (quot len 2)]
    (loop [i 0
           j (dec len)]
      (if (= i mid)
        true
        (when (= (.charAt s i) (.charAt s j))
          (recur (inc i) (dec j))))))) 


;;  rev = 0;
;;  while (num > 0)
;;  {
;;       dig = num % 10;
;;       rev = rev * 10 + dig;
;;       num = num / 10;
;;  }
 
;; If n == rev then num is a palindrome:

(defn palindrome?
  [num]
  (loop [n num
         reversed 0
         ]
    (if (<= n 0)
      (= num reversed)
      (recur (long (/ n 10)) (+ (* 10 reversed) (mod n 10))))))

;; http://rosettacode.org/wiki/Palindrome_detection#Clojure
(defn palindrome? [num]
  (let [s (str num)]
    (loop [i 0
           j (dec (. s length))]
      (cond (>= i j) true
            (= (get s i) (get s j))
            (recur (inc i) (dec j))
          :else false))))

;; OTHERS
;; http://stackoverflow.com/questions/199184/how-do-i-check-if-a-number-is-a-palindrome

;; https://searchcode.com/codesearch/view/67977972/
(def is-palindrom? (fn [x] 
                            (let
                              [strx (str x)
                               size (int (/ (count strx) 2))
                               lft (take size strx)
                               rht (take size (reverse strx))]
                              (= lft rht))))
(def  sub-generator (fn [x]
                      (if (sequential? x)
                        (let [[half is-odd?] x
                              rollover? (apply = (conj half \9))
                              half (seq (str (inc (bigint (apply str half)))))]
                          (if rollover?
                            (if is-odd?
                              [(drop-last half) (not is-odd?)]
                              [half (not is-odd?)])
                            [half is-odd?]))
                        (let [seqx (str x) ; Number as a sequence
                              size (count seqx) ; Number of digits in number
                              half (take (/ size 2) seqx)] ; First half of the number
                          (if (is-palindrom? x)
                            [(take (/ size 2) seqx) (odd? size)]
                            (if (> (bigint (apply str half)) ; First half is larger than second half
                                   (bigint (apply str (take (/ size 2) (reverse seqx)))))
                              [half (odd? size)]
                              [(seq (str (inc (bigint (apply str half))))) (odd? size)]))))))


(defn prob150
  [n]
    (filter palindrome? (iterate inc n)))


;; https://github.com/eigenhombre/probs4clojure/blob/master/test/probs4clojure/core_test.clj

(defn split-number
  [n]
  [(quot (inc n) 2), (quot n 2)])

(defn build 
  [s left right]
  (Long/parseLong (apply str (concat (take left s) (reverse (take right s))))))

(defn next-palindrome
  [n]
  (let [s (str n)
        [left _] (split-number (count s))
        next (inc (Long/parseLong (apply str (take left s))))
        [new-left new-right] (split-number (count (str (inc n))))
        ]
    (build (str next) new-left new-right)))

(defn problem-150
  [n]
  (drop-while #(< % n) (iterate next-palindrome n)))



(defn prob-150
  [n]
  (letfn [(split-number [n] [(quot (inc n) 2), (quot n 2)])
          (build [s left right] (Long/parseLong (apply str (concat (take left s) (reverse (take right s))))))
          (nearest-palindrome [x]
            (let [sx (str x)
                  [k1 k2] (split-number (count sx))]
              (build sx k1 k2)))
          (next-palindrome [n]
            (let [s (str n)
                  [left _] (split-number (count s))
                  next (inc (Long/parseLong (apply str (take left s))))
                  [new-left new-right] (split-number (count (str (inc n))))]
              (build (str next) new-left new-right)))]
    (drop-while #(< % n) (iterate next-palindrome (nearest-palindrome n)))))



(defn prob150-test []
  (and 
   (= (take 26 (prob150 0))
      [0 1 2 3 4 5 6 7 8 9 
       11 22 33 44 55 66 77 88 99 
       101 111 121 131 141 151 161])
   
   (= (take 16 (prob150 162))
      [171 181 191 202 
       212 222 232 242 
       252 262 272 282 
       292 303 313 323])
   
   (= (take 6 (prob150 1234550000))
      [1234554321 1234664321 1234774321 
       1234884321 1234994321 1235005321])
   
   (= (first (prob150 (* 111111111 111111111)))
      (* 111111111 111111111))
   
   (= (set (take 199 (prob150 0)))
      (set (map #(first (prob150 %)) (range 0 10000))))
   
   (= true 
      (apply < (take 6666 (prob150 9999999))))
   
   (= (nth (prob150 0) 10101)
      9102019)
   ))





;; 121
;; 
;; Return a function that accepts a map of parameters for the passed in function

(defn prob121
  [function]
  (fn [values]
    ;; apply the values to the function
    ((fn compute [s] 
       (if (seq? s)
         (let [[ op & args] s
               ;;  four basic mathematical operators
               fncs {'+ + '- - '/ / '* *}
               function (fncs op)]
           (apply function (map compute args)))
         (get values s s)))
     function)))


;; https://gist.github.com/amalloy/1231002
(fn [formula]
  (fn [values]
    ((fn compute [x]
       (if (seq? x)
         (let [[op & args] x]
           (apply ({'+ + '/ / '- - '* *} op)
                  (map compute args)))
         (get values x x)))
     formula)))


;; (= 2 ((__ '(/ a b))
;;       '{b 8 a 16}))


;; 112

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


;; 
(defn prob112
  [num xs]
  (letfn [(prob112-helper [[num] xs]
            (if (coll? xs)
              (let [rs (take-while #(<= 0 (first %))
                                   (reductions prob112-helper [num] xs))]
                [(first (last rs)) (map second (rest rs))])
              [(- num xs) xs]))]
    (second (prob112-helper [num] xs))))


;; 141
;; Tricky card games

;; {:suit :diamond :rank 10} 
;;
;; :club :diamond :spade :heart

;; [{:suit :spade :rank 2} {:suit :club :rank 10}]
;; [{:suit :heart :rank 6} {:suit :heart :rank 8} {:suit :diamond :rank 10} {:suit :heart :rank 4}]o(

(defn prob141
  [trump]
  (fn [trick]
    ;; if no trump is passed in use the suit of the first item in trick
    (let [suit (if trump trump (:suit (first trick)))]
      ;; filter by suit matching trick then sort by :rank to get highest card
      (last (sort-by :rank (filter #(= suit (:suit %)) trick))))))


;; 168
;; Inifinite Matrix

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

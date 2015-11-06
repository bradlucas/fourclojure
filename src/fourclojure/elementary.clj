(ns fourclojure.elementary)

;; ------------------------------------------------------------------------
;; Nothing but the Truth
;; Simple Math
;; Intro to Strings
;; Intro to Lists
;; Lists: conj
;; Intro to Vectors
;; Vectors: conj
;; Intro to Sets
;; Sets: conj
;; Intro to Maps
;; Maps: conj
;; Intro to Sequences
;; Sequences: rest
;; Intro to Functions
;; Double Down
;; Hello World
;; Sequences: map
;; Sequences: filter
;; Local bindings
;; Let it Be
;; Regular Expressions
;; Intro to Reduce
;; Simple Recursion
;; Rearranging Code: ->
;; Recurring Theme
;; Rearranging Code: ->>
;; A nil key
;; For the win
;; Logical falsity and truth
;; Subset and Superset
;; Map Defaults
;; Intro to Destructuring
;; ------------------------------------------------------------------------
;; ------------------------------------------------------------------------

 
;; Nothing but the Truth	
;; https://www.4clojure.com/problem/1
true

;; Simple Math	
;; https://www.4clojure.com/problem/2
4

;; Intro to Strings	
;; https://www.4clojure.com/problem/3
"HELLO WORLD"

;; Intro to Lists	
;; https://www.4clojure.com/problem/4
:a :b :c

;; Lists: conj	
;; https://www.4clojure.com/problem/5
'(1 2 3 4)

;; Intro to Vectors	
;; https://www.4clojure.com/problem/6
:a :b :c

;; Vectors: conj	
;; https://www.4clojure.com/problem/7
[1 2 3 4]

;; Intro to Sets	
;; https://www.4clojure.com/problem/8
#{:a :b :c :d}

;; Sets: conj	
;; https://www.4clojure.com/problem/9
2

;; Intro to Maps	
;; https://www.4clojure.com/problem/10
20

;; Maps: conj	
;; https://www.4clojure.com/problem/11
{:b 2}

;; Intro to Sequences	
;; https://www.4clojure.com/problem/12
3

;; Sequences: rest	
;; https://www.4clojure.com/problem/13
[20 30 40]

;; Intro to Functions	
;; https://www.4clojure.com/problem/14
8

;; Double Down
;; https://www.4clojure.com/problem/15
(fn [x] (+ x x))

;; Hello World	
;; https://www.4clojure.com/problem/16
(fn [x] (str "Hello, " x "!"))

;; Sequences: map	
;; https://www.4clojure.com/problem/17
'(6 7 8)

;; Sequences: filter	
;; https://www.4clojure.com/problem/18
'(6 7)

;; Local bindings
;; https://www.4clojure.com/problem/35
7

;; Let it Be
;; https://www.4clojure.com/problem/36
;; [x 7 y 3 z 1]

;; Regular Expressions
;; https://www.4clojure.com/problem/37
"ABC"

;; Intro to Reduce
;; https://www.4clojure.com/problem/64
+

;; Simple Recursion
;; https://www.4clojure.com/problem/57
'(5 4 3 2 1)

;; Rearranging Code: ->
;; https://www.4clojure.com/problem/71
last

;; Recurring Theme
;; https://www.4clojure.com/problem/68
'[7 6 5 4 3]

;; Rearranging Code: ->>
;; https://www.4clojure.com/problem/72
reduce +

;; A nil key
;; https://www.4clojure.com/problem/134
(fn prob134
  [k map]
   (if (contains? map k)
     (nil? (k map))
     (do (print "asdf") false)
     ))

;; For the win
;; https://www.4clojure.com/problem/145
'(1 5 9 13 17 21 25 29 33 37)

;; Logical falsity and truth
;; https://www.4clojure.com/problem/162
11

;; Subset and Superset
;; https://www.4clojure.com/problem/161
#{1 2}

;; Map Defaults
;; https://www.4clojure.com/problem/156
(fn prob156
  [num keys]
  (loop [[first & rest] keys
         final {}]
    (if (empty? rest)
      (conj final {first num})
      (recur rest (conj final {first num})))))

;; Intro to Destructuring
;; https://www.4clojure.com/problem/52
;; [c e]


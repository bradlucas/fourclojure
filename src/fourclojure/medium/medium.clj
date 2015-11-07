(ns fourclojure.medium.medium)

;; |------------------------------+--------------------------------------|
;; | Flipping out                 | https://www.4clojure.com/problem/46  |
;; | Rotate Sequence              | https://www.4clojure.com/problem/44  |
;; | Reverse Interleave           | https://www.4clojure.com/problem/43  |
;; | Split by Type                | https://www.4clojure.com/problem/50  |
;; | Count Occurrences            | https://www.4clojure.com/problem/55  |
;; | Find Distinct Items          | https://www.4clojure.com/problem/56  |
;; | Function Composition         | https://www.4clojure.com/problem/58  |
;; | Partition a Sequence         | https://www.4clojure.com/problem/54  |
;; | Juxtaposition                | https://www.4clojure.com/problem/59  |
;; | Word Sorting                 | https://www.4clojure.com/problem/70  |
;; | Prime Numbers                | https://www.4clojure.com/problem/67  |
;; | Filter Perfect Squares       | https://www.4clojure.com/problem/74  |
;; | Black Box Testing            | https://www.4clojure.com/problem/65  |
;; | Intro to Trampoline          | https://www.4clojure.com/problem/76  |
;; | Perfect Numbers              | https://www.4clojure.com/problem/80  |
;; | Anagram Finder               | https://www.4clojure.com/problem/77  |
;; | Sequence Reductions          | https://www.4clojure.com/problem/60  |
;; | Merge with a Function        | https://www.4clojure.com/problem/69  |
;; | intoCamelCase                | https://www.4clojure.com/problem/102 |
;; | Euler's Totient Function     | https://www.4clojure.com/problem/75  |
;; | Happy numbers                | https://www.4clojure.com/problem/86  |
;; | Reimplement Trampoline       | https://www.4clojure.com/problem/78  |
;; | The Balance of N             | https://www.4clojure.com/problem/115 |
;; | Power Set                    | https://www.4clojure.com/problem/85  |
;; | Equivalence Classes          | https://www.4clojure.com/problem/98  |
;; | Identify keys and values     | https://www.4clojure.com/problem/105 |
;; | Digits and bases             | https://www.4clojure.com/problem/137 |
;; | Sequence of pronunciations   | https://www.4clojure.com/problem/110 |
;; | Oscilrate                    | https://www.4clojure.com/problem/144 |
;; | Lazy Searching               | https://www.4clojure.com/problem/108 |
;; | Decurry                      | https://www.4clojure.com/problem/158 |
;; | Partially Flatten a Sequence | https://www.4clojure.com/problem/93  |
;; | Global take-while            | https://www.4clojure.com/problem/114 |
;; | Insert between two items     | https://www.4clojure.com/problem/132 |
;; | Write Roman Numerals         | https://www.4clojure.com/problem/104 |
;; | Generating k-combinations    | https://www.4clojure.com/problem/103 |
;; | Prime Sandwich               | https://www.4clojure.com/problem/116 |
;; | Universal Computation Engine | https://www.4clojure.com/problem/121 |
;; | The Big Divide               | https://www.4clojure.com/problem/148 |
;; | Intervals                    | https://www.4clojure.com/problem/171 |
;; | Sum Some Set Subsets         | https://www.4clojure.com/problem/131 |
;; | Balancing Brackets           | https://www.4clojure.com/problem/177 |
;; | Sequs Horribilis             | https://www.4clojure.com/problem/112 |
;; | Tricky card games            | https://www.4clojure.com/problem/141 |
;; | Palindromic Numbers          | https://www.4clojure.com/problem/150 |
;; | Infinite Matrix              | https://www.4clojure.com/problem/168 |
;; | Parentheses... Again         | https://www.4clojure.com/problem/195 |
;; |------------------------------+--------------------------------------|




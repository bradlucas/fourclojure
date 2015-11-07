(ns fourclojure.medium.problem-115)

;; The Balance of N
;; https://www.4clojure.com/problem/115
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



(ns fourclojure.medium.problem-137)

;; Digits and bases
;; https://www.4clojure.com/problem/137
;;
;; Write a function which returns a sequence of digits of a non-negative
;; number (first argument) in numerical system with an arbitrary base
;; (second argument). Digits should be represented with their integer
;; values, e.g. 15 would be [1 5] in base 10, [1 1 1 1] in base 2 and
;; [15] in base 16.

;; 15 = 1 * 10^1 + 5 * 10^0
;; 15 = (1 * 2^3) + (1 * 2^2) + (1 * 2^1) + (1 * 2^0)

;; (defn prob137
;;   [num base]
;;   [1 5]
;; )

(defn prob137
  [num base]
  (loop [n num
         acc ()]
    (if (zero? n)
      (if (empty? acc) '(0) acc)
      (recur (int (/ n base)) (conj acc (mod n base))))))

(= (prob137 15 10) [1 5])
(= (prob137 15 2) [1 1 1 1])

(defn ** [x n] (reduce * (repeat n x)))

;; 137
;; Function convert number to base. Return vector of digits
;;  e.g. 15 would be [1 5] in base 10, [1 1 1 1] in base 2 and [15] in base 16. 

(= [1 2 3 4 5 0 1] (prob137 1234501 10))
(= [1 0 0 1] (prob137 9 2))

[1 2 8]

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


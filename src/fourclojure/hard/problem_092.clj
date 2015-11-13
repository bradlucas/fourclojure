(ns fourclojure.hard.problem-092)

;; ----------------------------------------------------------------------------------------------------
;; Write a function to parse a Roman-numeral string and return the
;; number it represents.
;;
;; You can assume that the input will be well-formed, in upper-case,
;; and follow the subtractive principle. You don't need to handle any
;; numbers greater than MMMCMXCIX (3999), the largest number
;; representable with ordinary letters.
;;
;;
;; |--------+-------|
;; | Letter | Value |
;; |--------+-------|
;; | I      |     1 |
;; | V      |     5 |
;; | X      |    10 |
;; | L      |    50 |
;; | C      |   100 |
;; | D      |   500 |
;; | M      |  1000 |
;; |--------+-------|
;;
;; 
;; * RULES
;;
;; ** Repetition
;; A single letter may be repeated up to three times consecutively with
;; each occurrence of the value being additive.
;;
;; ** Additive Combination
;;
;; Larger numerals are placed to the left of the smaller numerals.
;;
;; ** Subtractive Combination
;;
;; A small value may be placed to the left of a larger value. When this
;; occurs the smaller number is subtracted from the larger.
;;
;; ** Repeated use of V, L and D
;;
;; Numbers which represent numbers begining with a '5' (V, L and D) may
;; only appear once in each numeral.
;;
;; You can't have VIV.
;;
;; ** Reducing values
;;
;; Comparing the value of each numeral as read from left to right should
;; never increase from one letter to the next.
;;
;; This means XIX is acceptable but XIM and IIV are not.
;; ----------------------------------------------------------------------------------------------------

(def roman-letter-map
  {
   :I 1
   :V 5
   :X 10
   :L 50
   :C 100
   :D 500
   :M 1000
   }
)

(defn get-val
  "Translate a Roman letter to it's decimal value"
  [l]
  (roman-letter-map (keyword (str l))))

(defn prob92
  [xs]
  (loop [acc 0
         xs xs]
    (println acc)
    (if (not (seq xs))
      acc
      (if (= (count xs) 1)
        (recur (+ acc (get-val (first xs))) (rest xs))
        (let [a (get-val (first xs))
              b (get-val (second xs))]
          (if (>= a b)
            (recur (+ acc a) (rest xs))
            (recur (- acc a) (rest xs))))
        )
      )
    )
)




;; Submitted
(defn prob92
  [xs]
  (let [roman-letter-map { :I 1, :V 5, :X 10, :L 50, :C 100, :D 500, :M 1000}
        get-val (fn [l] (roman-letter-map (keyword (str l))))]
    (loop [acc 0
           xs xs]
      (if (not (seq xs))
        acc
        (if (= (count xs) 1)
          (recur (+ acc (get-val (first xs))) (rest xs))
          (let [a (get-val (first xs))
                b (get-val (second xs))]
            (if (>= a b)
              (recur (+ acc a) (rest xs))
              (recur (- acc a) (rest xs))))
          )
        )
      )
    )
)

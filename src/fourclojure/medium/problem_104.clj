(ns fourclojure.medium.problem-104)

;; Write Roman Numerals
;; https://www.4clojure.com/problem/104

;; http://www.numericana.com/answer/roman.htm#valid
;; I=1, V=5, X=10, L=50, C=100, D=500, M=1000

;; (= [2 3 5 7 11 13]
;;   (__ 4 #(= 2 (mod % 3))
;;          [2 3 5 7 11 13 17 19 23]))
;; 104
;; Write Roman Numerals
;; (= "MMMCMXCIX" (__ 3999))

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


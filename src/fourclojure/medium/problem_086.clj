(ns fourclojure.medium.problem-086)

;; Happy numbers
;; https://www.4clojure.com/problem/86
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

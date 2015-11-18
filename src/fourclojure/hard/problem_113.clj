(ns fourclojure.hard.problem-113)

;; Write a function that takes a variable number of integer
;; arguments. If the output is coerced into a string, it should return
;; a comma (and space) separated list of the inputs sorted smallest to
;; largest. If the output is coerced into a sequence, it should return
;; a seq of unique input elements in the same order as they were
;; entered.


;; (= "1, 2, 3" (str (__ 2 1 3)))
;; (= '(2 1 3) (seq (__ 2 1 3 3 1 2)))

;; @see http://pepijndevos.nl/how-reify-works-and-how-to-write-a-custom-typ/
;; @see http://decomplecting.org/blog/2014/10/29/reify-this/

(defn prob113 
  [& params]
  (reify
    java.lang.Object
    (toString [this]
      ;; return comma separated list
      (apply str (interpose ", " (sort params))))
    clojure.lang.Seqable
    (seq [this]
      ;; return list of unique params in the same order as entered
      (seq (distinct params))
      )))

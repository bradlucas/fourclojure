(ns fourclojure.medium.problem-195)


;; Parentheses... Again 
;; https://www.4clojure.com/problem/195

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

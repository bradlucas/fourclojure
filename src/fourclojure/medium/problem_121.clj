(ns fourclojure.medium.problem-121)

;; Universal Computation Engine
;; https://www.4clojure.com/problem/121
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




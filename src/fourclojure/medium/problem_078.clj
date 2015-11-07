(ns fourclojure.medium.problem-078)

;; Reimplement Trampoline
;; https://www.4clojure.com/problem/78
(defn prob78 
  [f & args]
  (if (empty? args)
    (let [ret (f)]
      (if (fn? ret)
        (prob78 ret)
        ret))
    (prob78 #(apply f args))))

(defn foo []
  (letfn [(triple [x] #(sub-two (* 3 x)))
          (sub-two [x] #(stop?(- x 2)))
          (stop? [x] (if (> x 50) x #(triple x)))]
    (prob78 triple 2)))




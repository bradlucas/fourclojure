(ns fourclojure.medium.problem-114)

;; Global take-while
;; https://www.4clojure.com/problem/114
(defn prob114
  [n f xs]
  ;; loop over xs and count the number of times f[x] is true
  ;; top when you've counted n times
  (loop [cnt 0
         acc []
         xs xs]
    (if (= cnt n) 
      (butlast acc)
      (recur (if (f (first xs)) (inc cnt) cnt) (conj acc (first xs)) (rest xs)))))



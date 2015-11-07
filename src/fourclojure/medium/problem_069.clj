(ns fourclojure.medium.problem-069)

;; Merge with a Function
;; https://www.4clojure.com/problem/69
(defn prob69 [f & maps]
  (reduce
   (fn [m1 maps]
     (reduce
      (fn [map [key val]]
        (println map key val)
        (assoc map key (if (maps key) (f val (maps key)) val))
        )
      maps m1
      ))
   (first maps) (rest maps)))


(= (prob69 * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   {:a 4, :b 6, :c 20})


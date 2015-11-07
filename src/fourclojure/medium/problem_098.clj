(ns fourclojure.medium.problem-098)

;; Equivalence Classes
;; https://www.4clojure.com/problem/98
(defn prob98
  [f s]
  (set (map set (vals (group-by f s)))))

(= (prob98 #(* % %) #{-2 -1 0 1 2})
   #{#{0} #{1 -1} #{2 -2}})



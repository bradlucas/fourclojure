(ns fourclojure.medium.problem-065)

;; Black Box Testing
;; https://www.4clojure.com/problem/65

;; Figure out the input by testing it. Return :map, :set, :list, or :vector 
(defn prob65
  [col]
  (let [blank (empty col)]
    (cond
     (= blank {}) :map
     (= blank #{}) :set
     (= blank '()) (if (reversible? blank):vector :list)
     :else :unknown)))


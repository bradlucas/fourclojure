(ns fourclojure.medium.problem-050)

;; Split by type
;; https://www.4clojure.com/problem/50
(defn prob50 [col]
  (map second (group-by type col)))

(= (set (prob50 [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
(= (set (prob50 [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
(= (set (prob50 [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})


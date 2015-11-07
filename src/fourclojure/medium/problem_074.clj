(ns fourclojure.medium.problem-074)

;; Filter Prefect Squares
;; https://www.4clojure.com/problem/74
;; separate on commas into ints
;; filter perfect square
;; return comma separated list
(defn prob74 [comma-string]
  (let [string (clojure.string/split comma-string #",")
        nums (map read-string string)
        ]
    (apply str (interpose "," (filter #(let [x (Math/sqrt %)] (= (float (int x)) x))  nums)))))

;; perfect square
;; square too to x.0
;; if you truncate to an int and go back to float and still have  the same number you have x.0

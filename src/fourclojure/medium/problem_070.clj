(ns fourclojure.medium.problem-070)

;; Word Sorting
;; https://www.4clojure.com/problem/70
(defn prob70 [s]
  (let [cleaned (apply str (filter #(or (Character/isLetter %) (Character/isSpace %)(Character/isDigit %)) s))
        words (clojure.string/split cleaned #"\s")
        ]
    (sort #(.compareTo 
            (clojure.string/lower-case (read-string %1)) 
            (clojure.string/lower-case ( read-string %2))) words)))


(ns fourclojure.medium.problem-102)

;; intoCamelCase
;; https://www.4clojure.com/problem/102

;; split at hyphens and captialize subsequent words
(defn prob102
  [string]
  (let [words (clojure.string/split string #"-")
        num (count words)
        ]
    (if (= 1 num)
      (first words)
      (apply str (concat  [(first words)] (vec (map clojure.string/capitalize (rest words))))))))


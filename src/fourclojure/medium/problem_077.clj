(ns fourclojure.medium.problem-077)

;; Anagram Finder
;; https://www.4clojure.com/problem/77

;; (def words ["meat" "mat" "team" "mate" "eat"])
(defn prob77
  [words]

  ;; group-by sort words
  (group-by sort words)
  ;; {(\a \e \m \t) ["meat" "team" "mate"], (\a \m \t) ["mat"], (\a \e \t) ["eat"]}

  ;; pull out the values
  (map val (group-by sort words))
  ;; (["meat" "team" "mate"] ["mat"] ["eat"])

  ;; filter out the ones with at least two words
  (filter #(> (count %) 1) (map val (group-by sort words)))

  ;; put results into a set
  (into #{} (filter #(> (count %) 1) (map val (group-by sort words))))
)

(defn prob77
  [words]
  (into #{} (map set (filter #(> (count %) 1) (map val (group-by sort words))))))


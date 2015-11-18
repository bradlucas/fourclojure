(ns fourclojure.hard.problem-101)

;; Levenshtein Distance

;; http://rosettacode.org/wiki/Levenshtein_distance#Clojure
(defn levenshtein [str1 str2]
  (let [len1 (count str1)
        len2 (count str2)]
    (cond (zero? len1) len2
          (zero? len2) len1
          :else
          (let [cost (if (= (first str1) (first str2)) 0 1)]
            (min (inc (levenshtein (rest str1) str2))
                 (inc (levenshtein str1 (rest str2)))
                 (+ cost
                    (levenshtein (rest str1) (rest str2))))))))


;; memoize to help speed things up
;; @see https://gist.github.com/laurentpetit/828413

(defn prob101
  [a b]
  (let [levenshtein (fn [fnc str1 str2]
                     (let [len1 (count str1)
                           len2 (count str2)]
                       (cond (zero? len1) len2
                             (zero? len2) len1
                             :else
                             (let [cost (if (= (first str1) (first str2)) 0 1)]
                               (min (inc (fnc fnc (rest str1) str2))
                                    (inc (fnc fnc  str1 (rest str2)))
                                    (+ cost
                                       (fnc fnc (rest str1) (rest str2))))))))]
    (levenshtein (memoize levenshtein) a b)))

;; 

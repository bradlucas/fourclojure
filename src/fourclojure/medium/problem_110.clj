(ns fourclojure.medium.problem-110)

;; Sequence of pronunciations
;; https://www.4clojure.com/problem/110 |

;; lazy sequence of pronunciations
;; each element being a pronunciation of the previous element


;; [1 1] => [2 1] => "two ones" => "one two, one one"

;; @see https://twitter.com/fusupo/status/593484015199485953
;; @see https://www.refheap.com/100212

(defn probl10 [col] 
  (rest 
   (iterate (fn [x] (mapcat (fn [y] [(count y) (first y)]) (partition-by identity x)))
            col)))


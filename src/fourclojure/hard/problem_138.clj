(ns fourclojure.hard.problem-138)

;; Squares Squared

;; Build list of squares starting at start and ending at end
;; draw result padded has a rotated square

;; test
;; (= (prob138 3 81) [" 3 "
;;                    "1 9"
;;                    " 8 "])

;; (= (prob138 2 256) ["  6  "
;;                     " 5 * "
;;                     "2 2 *"
;;                     " 6 4 "
;;                     "  1  "])


;; length of numbers needs to equal or pad out to the next number 
;; 1x1 2x2 3x3 4x4
;; 1     4   9  16  (256)

;; required board size 
;; 1   3x3 5x5 7x7  (9x9)


 
;; | Board |    | Snake |    |
;; |-------+----+-------+----|
;; |   1x1 |  1 |       |  1 |
;; |   3x3 |  9 |   2x2 |  4 |
;; |   5x5 | 25 |   3x3 |  9 |
;; |   7x7 | 49 |   4x4 | 16 |
;; |       |    |       |    |


(defn squares-digit-list [start end]
  ;; (squares-digit-list 3 81) => (3 9 8 1)
  ;; if end is below the next then stop
  ;; (squares-digit-list 4 20) => (4 1 6)
  (let [square (fn [n] (* n n))
        digit-list (fn [nums]
                     ;; split numbers into single list of digits
                     (let [digits (fn [num] (map #(Character/digit % 10) (str num)))]
                       (flatten (map digits nums))))]
    (digit-list 
     (loop [acc [start]]
       (if (<= end (last acc))
         (drop-last acc)
         (recur (conj acc (square (last acc)))))))))

(defn pad [n coll val]
  ;; @see http://stackoverflow.com/a/27263793 
  (take n (concat coll (repeat val))))

(defn empty-board [size] (vec (repeat size (vec (repeat size nil)))))

;; (defn fours [] (let [** (fn [x n] (reduce * (repeat n x)))]
;;                  (map #(** 4 %) (range))))

(defn lengths [] 
  ;; (1 4 9 16 25 ...)
  (map #(* % %) (iterate inc 1)))

(defn board-sizes []
  ;; (1 3 5 7 9 ...)
  (iterate #(+ % 2) 1))


(defn pad-numbers
  [nums]
  (let [len (count nums)
        len' (first (filter #(>= % len) (lengths)))
        padded (pad len' nums \*)]
    padded))

(defn board-size 
  [nums]
  (first (filter #(> (* % %) (count (pad-numbers nums))) (board-sizes))))

(defn blank-board [nums]
  (empty-board (board-size nums)))

(defn write-board
  [xs]
  (vec (map (fn [x] (apply str (replace '{nil \space}  x))) xs)))

;; ----------------------------------------------------------------------------------------------------





(defn prob138
  [start end]
  (let [nums (squares-digit-list start end)
        padded (pad-numbers nums)
        board (blank-board nums)]
    (println nums)
    (println padded)
    (println board)
    ;; TODO write nums to the board
    (write-board board)
    ))





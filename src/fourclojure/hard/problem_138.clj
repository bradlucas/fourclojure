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
       (if (< end (last acc))
         (drop-last acc)
         (recur (conj acc (square (last acc)))))))))


;; (defn fours [] (let [** (fn [x n] (reduce * (repeat n x)))]
;;                  (map #(** 4 %) (range))))

(defn lengths [] 
  ;; lengths of the strings
  ;; (1 4 9 16 25 ...)
  ;; 
  (map #(* % %) (iterate inc 1)))

(defn board-sizes []
  ;; (1 3 5 7 9 ...)
  ;; or
  ;; 1x1, 3x3, 5x5, 7x7, ....
  ;; 1, 9, 25, 49
  (iterate #(+ % 2) 1))

(defn pad [n coll val]
  ;; @see http://stackoverflow.com/a/27263793 
  (take n (concat coll (repeat val))))

(defn pad-numbers
  [nums]
  (let [len (count nums)
        len' (first (filter #(>= % len) (lengths)))
        padded (pad len' nums \*)]
    padded))

(defn board-size 
  [nums]
  (let [padded-length (count (pad-numbers nums))]
    (second (first (filter #(= (first %) padded-length) (map vector (lengths) (board-sizes)))))))

(defn empty-board [size] (vec (repeat size (vec (repeat size nil)))))

(defn blank-board [nums]
  (empty-board (board-size nums)))

(defn write-board
  [xs]
  (vec (map (fn [x] (apply str (replace '{nil \space}  x))) xs)))

;; ----------------------------------------------------------------------------------------------------
;; Snake Idea (11-25-2015)
;; ----------------------------------------------------------------------------------------------------
(defn start-position
  [board]
  ;; where should be start
  ;; 1x1 board => [0 0]
  ;; 3x3 board => [0 1]
  ;; 5x5 board => [2 2]
  ;; 7x7 board => [2 3]
  (case (count board)
    1 [0 0]
    3 [0 1] ;;  [1 0] ;; [0 1]
    5 [2 2] ;; [3 1] ;; [2 2]
    7 [2 3]
    nil   ;; TODO this function should be able to handle all sizes (formula)
    )
)

(defn direction-value
  [direction]
  (
   {:down-right [1 1]
    :down-left  [1 -1]
    :up-left    [-1 -1]
    :up-right   [-1 1]
    } direction)
  )

(defn turn-right
  [direction]
  ;; facing in the 'direction' return the point to the right
  (case direction
    :down-right :down-left
    :down-left :up-left
    :up-left :up-right
    :up-right :down-right))

(defn move-point
  [board point new-direction]
  ;; return the point in the 'new-direction'
  ;; apply new-direction to point
  (map + point (direction-value new-direction)))

(defn look-right 
  [board point direction]
  ;; return the value to our right
  (get-in board (move-point board point (turn-right direction))))

(defn next-direction 
  [board point direction]
  ;; can we turn right?
  (let [right-value (look-right board point direction)]
    (if (nil? right-value)
      (turn-right direction)
      ;; else continue in the current-direction
      direction)))

(defn update-board
  [board point value]
  (assoc-in board point value))

(defn snake
  [board lst]
  ;; write the lst into the board
  ;; 1. find the starting point
  (let [start-point (start-position board)]
    ;; 2. write starting point and recurse over board moving through list
    ;; - first move down-right
    ;; - loop
    ;; - can i turn right?
    ;;   - turn and move
    ;;   - continue
    
    (loop [board (update-board board start-point (first lst))
           lst (rest lst)
           current-point start-point
           direction :up-right]
      (println current-point " " direction)
      (println board)
      (if (empty? lst)
        board
        ;; next-point is right of 'point' or next in 'direction'
        (let [new-direction (next-direction board current-point direction)]
          (let [new-point (move-point board current-point new-direction)]
            (println new-point " " new-direction)
            (recur (update-board board new-point (first lst)) (rest lst) new-point new-direction)))))))

;; ----------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------


(defn prob138
  [start end]
  (let [digits (squares-digit-list start end)
        board (empty-board (board-size digits))
        lst (pad-numbers digits)
        ]
    (write-board (snake board lst))
    )
)

(= (prob138 2 2) ["2"])
(= (prob138 2 4) [" 2 "
                  "* 4"
                  " * "])
(= (prob138 3 81) [" 3 "
                   "1 9"
                   " 8 "])
(= (prob138 4 20) [" 4 "
                   "* 1"
                   " 6 "])
(= (prob138 2 256) ["  6  "
                    " 5 * "
                    "2 2 *"
                    " 6 4 "
                    "  1  "])
(= (prob138 10 10000) ["   0   "
                       "  1 0  "
                       " 0 1 0 "
                       "* 0 0 0"
                       " * 1 * "
                       "  * *  "
                       "   *   "])

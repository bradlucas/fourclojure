(ns fourclojure.hard.problem-094)

;; Game of Life
;; http://www.4clojure.com/problem/94
;;
;; The game of life is a cellular automaton devised by mathematician John Conway. (https://en.wikipedia.org/wiki/Conway's_Game_of_Life)
;;
;; The 'board' consists of both live (#) and dead ( ) cells. Each cell interacts with its eight neighbours (horizontal, vertical, diagonal), and its next state is dependent on the following rules:
;;
;; 1) Any live cell with fewer than two live neighbours dies, as if caused by under-population.
;; 2) Any live cell with two or three live neighbours lives on to the next generation.
;; 3) Any live cell with more than three live neighbours dies, as if by overcrowding.
;; 4) Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
;;
;;
;; Write a function that accepts a board, and returns a board representing the next generation of cells.

(defn decode-row 
  "Decode a string representing a row consiting of live (#) and dead ( ) cells.
  Translate to nil for dead and :on for live
  (decode-row \" ##   \") => [nil :on :on nil nil nil]"
  [row]
  (vec (replace '{\space nil \# :on} (seq row) )))

(defn encode-row
  "Reverse an encoded sequence
  (encode-row [nil :on :on nil nil nil]) => \" ##   \""
  [xs]
  (apply str (replace '{nil \space :on \#} xs)))

(defn load-from-strings
  "Return the 'board' after decoding the string rows"
  [strs]
  ;; count is the height
  (vec (map #(decode-row %) strs)))

(defn write-to-strings
  [xs]
  (vec (map #(encode-row %) xs)))

(defn get-live-cells
  "For a given 'board' return the set of live cells [r c]"
  [board]
  (for [r (range 6)
        c (range 6)
        :when (is-live board [r c])]
    [r c]))

(defn neighbors
  "Return the neighboring cells for a given cell [r c]"
  [[x y]]
  ;; https://en.wikipedia.org/wiki/Moore_neighborhood
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= 0 dx dy)] ;; (not (= dx dy)) -> ignore dx=dy=0
    [(+ x dx) (+ y dy)]))

(defn is-live
  "True if cell [r c] is live"
  [board [r c]]
  (= :on (get-in board [r c])))  ;; [row column] => [height width]

(defn live-neighbor-count
  "Find the number of live neighboars for a cell [r c]"
  [board [r c]]
  (let [neighbors (neighbors [r c])]
    (count (filter #(is-live board %) neighbors))))


(defn transform
  "Transform the board to the next step and return the set of new live cells"
  [board]
  ;; if 3 neighbors 
  ;;     dead because live
  ;;     live lives
  ;;
  ;; if 2 neighbors
  ;;     live lives
  (filter 
   #(not (nil? %))  
   (let [live-cells (get-live-cells board)]
     (for [r (range 6) c (range 6)]
       (let [cnt (live-neighbor-count board [r c])]
         (if (or (= 3 cnt)
                 (and (= 2 cnt)
                      (is-live board [r c])))
           [r c]))))))

(defn empty-board
  [size]
  (vec (repeat size (vec (repeat size nil)))))

(defn build-board
  "Build a new board from a set of live cells"
  [size live-cells]
  (loop [b (empty-board size)
         live-cells live-cells]
    (if (empty? live-cells)
      b
      (recur (assoc-in b (first live-cells) :on) (rest live-cells)))))


;; To get the dimension of the board
;; get [h w] ...
;; (map count ((juxt identity first) (vec (map vec i))))

;; ----------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------

(def i ["      " " ##   " " ##   " "   ## " "   ## " "      "])
(def b (load-from-strings i))
(= i (write-to-strings b))

(def r ["      " " ##   " " #    " "    # " "   ## " "      "]) 

;; (= (__ ["      "  
;;         " ##   "
;;         " ##   "
;;         "   ## "
;;         "   ## "
;;         "      "])
;;    ["      "  
;;     " ##   "
;;     " #    "
;;     "    # "
;;     "   ## "
;;     "      "])

;; ----------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------

(defn prob94
  [strs]
  (letfn [
          ;; Decode a string representing a row consiting of live (#) and dead ( ) cells.
          ;;   Translate to nil for dead and :on for live
          ;;   (decode-row \" ##   \") => [nil :on :on nil nil nil]
          (decode-row [row]
            (vec (replace '{\space nil \# :on} (seq row) )))

          ;; Reverse an encoded sequence
          ;;   (encode-row [nil :on :on nil nil nil]) => \" ##   \"
          (encode-row [xs]
            (apply str (replace '{nil \space :on \#} xs)))

          ;; Return the 'board' after decoding the string rows
          (load-from-strings [strs]
            ;; count is the height
            (vec (map #(decode-row %) strs)))

          (write-to-strings [xs]
            (vec (map #(encode-row %) xs)))

          ;; For a given 'board' return the set of live cells [r c]
          (get-live-cells [board]
            (for [r (range 6)
                  c (range 6)
                  :when (is-live board [r c])]
              [r c]))

          ;; Return the neighboring cells for a given cell [r c]
          (neighbors [[x y]]
            ;; https://en.wikipedia.org/wiki/Moore_neighborhood
            (for [dx [-1 0 1]
                  dy [-1 0 1]
                  :when (not= 0 dx dy)] ;; (not (= dx dy)) -> ignore dx=dy=0
              [(+ x dx) (+ y dy)]))

          ;; True if cell [r c] is live
          (is-live [board [r c]]
            (= :on (get-in board [r c])))  ;; [row column] => [height width]

          ;; Find the number of live neighboars for a cell [r c]
          (live-neighbor-count [board [r c]]
            (let [neighbors (neighbors [r c])]
              (count (filter #(is-live board %) neighbors))))

          ;; Transform the board to the next step and return the set of new live cells
          (transform [board]
            ;; if 3 neighbors 
            ;;     dead because live
            ;;     live lives
            ;;
            ;; if 2 neighbors
            ;;     live lives
            (filter 
             #(not (nil? %))  
             (let [live-cells (get-live-cells board)]
               (for [r (range 6) c (range 6)]
                 (let [cnt (live-neighbor-count board [r c])]
                   (if (or (= 3 cnt)
                           (and (= 2 cnt)
                                (is-live board [r c])))
                     [r c]))))))

          (empty-board
            [size]
            (vec (repeat size (vec (repeat size nil)))))
   
          ;; Build a new board from a set of live cells
          (build-board [size live-cells]
            (loop [b (empty-board size)
                   live-cells live-cells]
              (if (empty? live-cells)
                b
                (recur (assoc-in b (first live-cells) :on) (rest live-cells)))))
          ]
    (let [board (load-from-strings strs)]
      (write-to-strings (build-board (count board) (transform board))))))

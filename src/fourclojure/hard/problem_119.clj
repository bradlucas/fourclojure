>(ns fourclojure.hard.problem-119)

;; ----------------------------------------------------------------------------------------------------
;; similar to 73

;; players are :x and :o and an empty position is :e

(def board
  [[:o :e :e] 
   [:o :x :o] 
   [:x :x :e]])

;; For a given board get all the open positions [r c]
(defn open-positions
  "Return the row column pairs [r c] for the positions that contain an empty (:e)"
  [board]
  (for [r (range 3)
        c (range 3)
        :when (= (get-in board [r c]) :e)]
    [r c]))

;; For each try setting the position in the board for the player
;; and evaluate the board to see if the player has won
(defn row [m n] (nth m n))

(defn rows [m]  (map #(row m %) (range 3)))

(defn col [m n] (vec (map #(nth % n) m)))

(defn cols [m]  (map #(col m %) (range 3)))

(defn diags [m] 
  (list [(last (col m 0)) (second (col m 1)) (first (col m 2))]
        [(first (col m 0)) (second (col m 1)) (last (col m 2))]))

(defn get-all-tuples
  "Return all the columns, rows and diagonals from a given board"
  [board]
  (concat (rows board) (cols board) (diags board)))

(defn solved-tuple
  "Return true if the position contains all three positions for the player"
  [player position]
  (= 3 (count (filter #(= player %) position))))

(defn solved-board
  "Return true if the board has been solved. In other words there is a position that wins"
  [player board]
  (= 1 (count (filter #(solved-tuple player %) (get-all-tuples board)))))

(defn update-board
  [player board position]
  (let [[r c] position]
    (assoc-in board [r c] player)))

(defn solves-board?
  [player board position]
  ;; apply position to board and see if the player wins
  (let [new-board (update-board player board position)]
    (solved-board player new-board)))

(defn prob115
  [player board]
  (set (filter #(solves-board? player board %) (open-positions board))))



;; ----------------------------------------------------------------------------------------------------


(fn prob115
  [player board]

  (letfn [
          (open-positions
            ;; For a given board get all the open positions [r c]
            ;; Return the row column pairs [r c] for the positions that contain an empty (:e)
            [board]
            (for [r (range 3)
                  c (range 3)
                  :when (= (get-in board [r c]) :e)]
              [r c]))
          
          ;; For each try setting the position in the board for the player
          ;; and evaluate the board to see if the player has won
          (row [m n] (nth m n))
          (rows [m]  (map #(row m %) (range 3)))
          (col [m n] (vec (map #(nth % n) m)))
          (cols [m]  (map #(col m %) (range 3)))
          (diags [m] 
            (list [(last (col m 0)) (second (col m 1)) (first (col m 2))]
                  [(first (col m 0)) (second (col m 1)) (last (col m 2))]))
          
          (get-all-tuples
            ;; Return all the columns, rows and diagonals from a given board
            [board]
            (concat (rows board) (cols board) (diags board)))
          
          (solved-tuple
            ;; Return true if the position contains all three positions for the player
            [player position]
            (= 3 (count (filter #(= player %) position))))
          
          (solved-board
            ;; Return true if the board has been solved. In other words there is a position that wins
            [player board]
            (= 1 (count (filter #(solved-tuple player %) (get-all-tuples board)))))
          
          (update-board
            [player board position]
            (let [[r c] position]
              (assoc-in board [r c] player)))

          (solves-board?
            [player board position]
            ;; apply position to board and see if the player wins
            (let [new-board (update-board player board position)]
              (solved-board player new-board)))
          ]
    (set (filter #(solves-board? player board %) (open-positions board)))))

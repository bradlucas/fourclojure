(ns fourclojure.hard.problem-124)

;; Analyze Reversi

;; 4x4 board
;; black (b), white (w) and empty (e) pieces
;; 
;; function should accept a game board and a color
;; return all the legal moves for that color
;; 
;;
;; find e which if replace with color wraps your competitor color
;; for example,
;; [w b e] => w b w => [w w w] 
;; return the position where you are putting a piece down
;; and the position of the prieces that need to be flipped

;; (= {[1 3] #{[1 2]}, [0 2] #{[1 2]}, [3 1] #{[2 1]}, [2 0] #{[2 1]}}
;;    (__ '[[e e e e]
;;          [e w b e]
;;          [e b w e]
;;          [e e e e]] 'w))

;; get all the rows, columns and diagonals
;; for each replace e with the color and evaluate the row
;; if the new row encapsulates the competitor then 

;; ----------------------------------------------------------------------------------------------------

;; ----------------------------------------------------------------------------------------------------
;; Testing

(def b '[[e e e e]
         [e w b e]
         [e b w e]
         [e e e e]])

;; answer for 'w
(def answer {[1 3] #{[1 2]}, 
             [0 2] #{[1 2]}, 
             [3 1] #{[2 1]}, 
             [2 0] #{[2 1]}})

(= {[1 3] #{[1 2]}, [0 2] #{[1 2]}, [3 1] #{[2 1]}, [2 0] #{[2 1]}}
   (prob124 '[[e e e e]
         [e w b e]
         [e b w e]
         [e e e e]] 'w))

(def row [[[0 0] :e] [[0 1] :e] [[0 2] :e] [[0 3] :e]])

;; ----------------------------------------------------------------------------------------------------

;; For all open positions on the board
;; Try in each direction to find potential captured pieces

(defn directions []
  ;; return all the directions including diagonals
  (for [r [-1 0 1]
        c [-1 0 1]
        :when (not= r c 0)]
    [r c])

  ;; [[0 -1]]
)

(defn walk-to-end [pos dir dim]
  ;; return the seq from a given pos to the end of the board
  ;; dim is the size of the board

  ;; ensure we don't have a pos with -1 or dim values
  (letfn [(valid? ([[r c]] (and (> r -1) (< r dim) (> c -1) (< c dim))))]
    ;; return all the the current pos
    (rest (take-while valid? (iterate #(map + dir %) pos)))))

(defn opposite-color [color]
  (cond
    (= 'w color) 'b
    (= 'b color) 'w))

(defn is-opponent [board color pos]
  (let [opp (opposite-color color)]
    (= opp (get-in board pos))))

(defn find-empty-positions [board]
  ;; assumming square board
  (let [dim (count board)]
    ;; iterate over board returning all 'e positions
    (for [r (range dim)
          c (range dim)
          :when (= 'e (get-in board [r c]))]
      [r c])))

(defn search [board color pos]
  ;; this filter looks crufty. it removes the empty nil searches
  (filter #(not (nil? %))
   ;; for each direction          
   (for [dir (directions)]
     ;; walk to end from pos and return all the board contents
     ;; the split-with ideas comes from amalloy's solution
     ;; @see https://gist.github.com/amalloy/1244458
     (let [contents (walk-to-end pos dir (count board))
           [captured end] (split-with #(is-opponent board color %) contents)]
       ;; if end is your color
       ;; if every in captured is opposite of your color
       (if (and (= color (get-in board (first end)))
                (seq captured)
                (every? #(is-opponent board color %) captured))
         [pos captured]
         ))
     )
   )
)

(defn prob124 [board color]
  (let [empty-positions (find-empty-positions board)
        dim (count board)]
      ;; this filter looks crufty. it removes the empty nil searches
    (let [res (filter #(not (every? empty? %))
                      ;; search the board for each empty position
                      (for [pos empty-positions] 
                        (search board color pos)))]
      ;; return our results in a map with position as key and value the turned positions
      (into {} (map (fn [r] (let [[pos vals] (first r)] {pos (set vals)})) res))
      )
    )
)


;; ----------------------------------------------------------------------------------------------------



(defn prob124 [board color]
  (letfn [(directions []
            ;; return all the directions including diagonals
            (for [r [-1 0 1]
                  c [-1 0 1]
                  :when (not= r c 0)]
              [r c])
            
            ;; [[0 -1]]
            )
          
          (walk-to-end [pos dir dim]
            ;; return the seq from a given pos to the end of the board
            ;; dim is the size of the board
            
            ;; ensure we don't have a pos with -1 or dim values
            (letfn [(valid? ([[r c]] (and (> r -1) (< r dim) (> c -1) (< c dim))))]
              ;; return all the the current pos
              (rest (take-while valid? (iterate #(map + dir %) pos)))))
          
          (opposite-color [color]
            (cond
              (= 'w color) 'b
              (= 'b color) 'w))
          
          (is-opponent [board color pos]
            (let [opp (opposite-color color)]
              (= opp (get-in board pos))))
          
          (find-empty-positions [board]
            ;; assumming square board
            (let [dim (count board)]
              ;; iterate over board returning all 'e positions
              (for [r (range dim)
                    c (range dim)
                    :when (= 'e (get-in board [r c]))]
                [r c])))
          
          (search [board color pos]
            ;; this filter looks crufty. it removes the empty nil searches
            (filter #(not (nil? %))
                    ;; for each direction          
                    (for [dir (directions)]
                      ;; walk to end from pos and return all the board contents
                      ;; the split-with ideas comes from amalloy's solution
                      ;; @see https://gist.github.com/amalloy/1244458
                      (let [contents (walk-to-end pos dir (count board))
                            [captured end] (split-with #(is-opponent board color %) contents)]
                        ;; if end is your color
                        ;; if every in captured is opposite of your color
                        (if (and (= color (get-in board (first end)))
                                 (seq captured)
                                 (every? #(is-opponent board color %) captured))
                          [pos captured]
                          )))))]
    (let [empty-positions (find-empty-positions board)
          dim (count board)]
      ;; this filter looks crufty. it removes the empty nil searches
      (let [res (filter #(not (every? empty? %))
                        ;; search the board for each empty position
                        (for [pos empty-positions] 
                          (search board color pos)))]
        ;; return our results in a map with position as key and value the turned positions
        (into {} (map (fn [r] (let [[pos vals] (first r)] {pos (set vals)})) res))
        ))))

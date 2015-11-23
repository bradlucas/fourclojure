(ns fourclojure.hard.problem-117)

;; For Science!

;; spaces represent areas where the mouse can walk freely
;; hashes (#) represent walls where the mouse can not walk
;; M represents the mouse's starting point
;; C represents the cheese which the mouse must reach

(def m ["#######"
        "#     #"
        "#  #  #"
        "#M # C#"
        "#######"])

;; find position of M
;; find position of C
;; path along spaces

;; The mouse is not allowed to travel diagonally in the maze (only
;; up/down/left/right), nor can he escape the edge of the maze. Your
;; function must return true iff the maze is solvable by the mouse.

;; convert maze spaces to positional [r c]

;; evaluate if you can move from M -> C using these positions

;; ----------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------


(defn prob117
  [maze]
    (letfn [
            ;; Return the neighboring cells for a given cell [r c]. Do not return diagnals.
            ;;   For example, for the cell [3 1] return ([2 1] [3 0] [3 2] [4 1])
            ;;         [2 1]
            ;;   [3 0] [3 1] [3 2]
            ;;         [4 1]
            (neighbors
              [[x y]]
              (for [dx [-1 0 1]
                    dy [-1 0 1]
                    :when (and (or (= 0 dx) (= 0 dy))
                               (not= 0 dx dy))]      
                [(+ x dx) (+ y dy)]))

            ;; A valid neighbor is one which contains a space or a C
            (valid-neighbors
              [m [r c]]
              (letfn [(valid [[r c]]
                        (let [v (get-in m [r c])] 
                          (or (= v \space) 
                              (= v \C))))]
                (filter valid (neighbors [r c]))))
            
            ;; Find the first location with a given value. Used to find the mouse and the cheese
            (find-element
              [m val]
              (first 
               (for [r (range (count m))
                     c (range (count (first m)))
                     :when (= val (get-in m [r c]))]
                 [r c])))

            ;; Return the location of the mouse M
            (m-pos 
              [m]
              (find-element m \M))

            ;; Return the location of the cheese C
            (c-pos 
              [m]
              (find-element m \C))
            
            (search [[path] [r c]]
              (if (= (c-pos maze) [r c])
                [(conj path [r c])]  ;; found it
                (let [neighbors' (valid-neighbors maze [r c])                ;; try neighbors
                      neighbors (filter #(nil? (some #{%} path)) neighbors')]
                  (if (seq neighbors)
                    (reduce #(search %1 %2) [(conj path [r c])] neighbors)
                    [(conj path [r c])]))
                ))]
      (let [mouse (m-pos maze)
            cheese (c-pos maze)]
           (not (empty? (filter #(= cheese %) (first (search [] mouse)))))
        )))





;; ----------------------------------------------------------------------------------------------------
;; test cases
;; ----------------------------------------------------------------------------------------------------

(= true  (prob117 ["M   C"]))
(= false (prob117 ["M # C"]))
(= true  (prob117 ["#######"
                   "#     #"
                   "#  #  #"
                   "#M # C#"
                   "#######"]))

(= false (prob117 ["########"
                   "#M  #  #"
                   "#   #  #"
                   "# # #  #"
                   "#   #  #"
                   "#  #   #"
                   "#  # # #"
                   "#  #   #"
                   "#  #  C#"
                   "########"]))

(= false (prob117 ["M     "
                   "      "
                   "      "
                   "      "
                   "    ##"
                   "    #C"]))

(= true  (prob117 ["C######"
                   " #     "
                   " #   # "
                   " #   #M"
                   "     # "]))

(= true  (prob117 ["C# # # #"
                   "        "
                   "# # # # "
                   "        "
                   " # # # #"
                   "        "
                   "# # # #M"]))


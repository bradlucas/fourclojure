(ns fourclojure.hard.problem-111)

;; Crossword puzzle


;; Write a function that takes a string and a partially-filled crossword puzzle board, and determines if the input string can be legally placed onto
;; the board.

;; The crossword puzzle board consists of a collection of partially-filled rows. Empty spaces are denoted with an underscore (_), unusable spaces are
;; denoted with a hash symbol (#), and pre-filled spaces have a character in place; the whitespace characters are for legibility and should be
;; ignored.

;; For a word to be legally placed on the board:
;; - It may use empty spaces (underscores)
;; - It may use but must not conflict with any pre-filled characters.
;; - It must not use any unusable spaces (hashes).
;; - There must be no empty spaces (underscores) or extra characters before or after the word (the word may be bound by unusable spaces though).
;; - Characters are not case-sensitive.
;; - Words may be placed vertically (proceeding top-down only), or horizontally (proceeding left-right only).


;; Try and fit the word into the puzzle
;; The word fits if it overlays either spaces _ or matching characters

;; (prob111 "the" ["_ # _ _ e"]) is true because ["_ # t h e"]

;; You can go top down on columns as well as left to right on rows


;; Ideas:
;; try all rows
;; try all columns (if size >= length of word)

;; need some sort of matcher that lays the word over each position

;; ----------------------------------------------------------------------------------------------------

(defn remove-spaces [s]
  (filter #(not= \space %) (seq s)))

(defn decode-str
  [row]
  (vec (replace '{\_ nil \# :off}  (remove-spaces row))))

(defn rows [board]
  (map decode-str board))

(defn cols [board]
  (map decode-str (filter not-empty 
                          (map #(remove-spaces %) 
                               ;; return the columns if they are at least size in length
                               (let [width (count (first board))
                                     height (count board)]
                                 ;; return the column of value [0 0] [1 0] [2 0] ..., [1 0] [1 1] [1 2] ...
                                 (for [c (range width)]
                                   (vec 
                                    (for [r (range height)]
                                      (get-in board [r c])))))))))

(defn rows-cols [board]
  (concat (rows board) (cols board)))

(defn match [a b]
  ;; for each character
  ;; matches if a == b or b == nil
  (every? true? (map (fn [a b] (or (= nil b) (= a b))) a b)))

(defn mid-matcher [fnc word rowcol len]
  ;; rotate through a window (mid) with length len
  ;; and return all cases where the mid matches
  ;; lh mid rh
    (let [rowcol (vec rowcol)]
      (loop [acc []
             lh []
             mid (vec (take len rowcol))
             rh (vec (drop len rowcol))]
        (if (nil? rh)
          acc
          (let [acc (if (fnc word mid) (conj acc [lh mid rh]) acc)]
            (recur acc
                   (conj lh (first mid)) 
                   (conj (vec (rest mid)) (first rh)) 
                   (if (empty? rh) nil (rest rh))))))))

(defn neighbor-check [lh rh]
  ;; it is ok if the neighbors are # or don't exist
  (and (or (empty? lh)
          (= :off (last lh)))
       (or (empty? rh)
          (= :off (first rh)))))

(defn word-fits [word rowcol]
  ;; can you fit word inside of rowcol?
  (let [word (seq word)
        len (count word)]
    (if (< (count rowcol) len)
      false   ;; not long enough
      (let [lst (mid-matcher match word rowcol len)]
        (if lst
          ;; any valid neighbor situations
          (some (fn [[lh mid rh]] (neighbor-check lh rh)) lst))))))
 
(defn prob111 [word board]
  (let [len (count word)
        rowcols (rows-cols board)]
    (some #(word-fits word %) rowcols)))

;; ----------------------------------------------------------------------------------------------------
;; Testing
(comment 
  (= true (prob111 "the" ["_ # _ _ e"]))  ;; # the

  (= false (prob111 "the" ["c _ _ _"       ;; can't have c the
                           "d _ # e"
                           "r y _ _"]))

  (= true  (prob111 "joy" ["c _ _ _"      ;; vertical joy
                           "d _ # e"
                           "r y _ _"]))

  (= false (prob111 "joy" ["c o n j"      
                           "_ _ y _"     ;; can't have joy _
                           "r _ _ #"]))

  (= true  (prob111 "clojure" ["_ _ _ # j o y"
                               "_ _ o _ _ _ _"    ;; clojure
                               "_ _ f _ # _ _"]))
)


;; ----------------------------------------------------------------------------------------------------



(defn prob111 [word board]
  (letfn [(remove-spaces [s]
            (filter #(not= \space %) (seq s)))

          (decode-str
            [row]
            (vec (replace '{\_ nil \# :off}  (remove-spaces row))))
          
          (rows [board]
            (map decode-str board))
          
          (cols [board]
            (map decode-str (filter not-empty 
                                    (map #(remove-spaces %) 
                                         ;; return the columns if they are at least size in length
                                         (let [width (count (first board))
                                               height (count board)]
                                           ;; return the column of value [0 0] [1 0] [2 0] ..., [1 0] [1 1] [1 2] ...
                                           (for [c (range width)]
                                             (vec 
                                              (for [r (range height)]
                                                (get-in board [r c])))))))))
          
          (rows-cols [board]
            (concat (rows board) (cols board)))
          
          (match [a b]
            ;; for each character
            ;; matches if a == b or b == nil
            (every? true? (map (fn [a b] (or (= nil b) (= a b))) a b)))
          
          (mid-matcher [fnc word rowcol len]
            ;; rotate through a window (mid) with length len
            ;; and return all cases where the mid matches
            ;; lh mid rh
            (let [rowcol (vec rowcol)]
              (loop [acc []
                     lh []
                     mid (vec (take len rowcol))
                     rh (vec (drop len rowcol))]
                (if (nil? rh)
                  acc
                  (let [acc (if (fnc word mid) (conj acc [lh mid rh]) acc)]
                    (recur acc
                           (conj lh (first mid)) 
                           (conj (vec (rest mid)) (first rh)) 
                           (if (empty? rh) nil (rest rh))))))))
          
          (neighbor-check [lh rh]
            ;; it is ok if the neighbors are # or don't exist
            (and (or (empty? lh)
                     (= :off (last lh)))
                 (or (empty? rh)
                     (= :off (first rh)))))
          
          (word-fits [word rowcol]
            ;; can you fit word inside of rowcol?
            (let [word (seq word)
                  len (count word)]
              (if (< (count rowcol) len)
                false   ;; not long enough
                (let [lst (mid-matcher match word rowcol len)]
                  (if lst
                    ;; any valid neighbor situations
                    (some (fn [[lh mid rh]] (neighbor-check lh rh)) lst))))))]
    (let [len (count word)
          rowcols (rows-cols board)]
      (true? (some #(word-fits word %) rowcols)))))

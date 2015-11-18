(ns fourclojure.hard.problem-178)

;; Recognizing Cards
;;
;; Following on from Recognize Playing Cards (;; https://www.4clojure.com/problem/128)
;; determine the best poker hand that can be made with five cards. 
;;
;; The hand rankings are listed below for your convenience.
;;
;; - Straight flush: All cards in the same suit, and in sequence
;; - Four of a kind: Four of the cards have the same rank
;; - Full House: Three cards of one rank, the other two of another rank
;; - Flush: All cards in the same suit
;; - Straight: All cards in sequence (aces can be high or low, but not both at once)
;; - Three of a kind: Three of the cards have the same rank
;; - Two pair: Two pairs of cards have the same rank
;; - Pair: Two cards have the same rank
;; - High card: None of the above conditions are met


(defn card-suit-rank [s]
    (letfn [(suit [c]
              ({
                \D :diamond
                \H :heart
                \C :club
                \S :spade
                } 
               c)
              )
            (rank [c]
              ({
                \2 0
                \3 1
                \4 2
                \5 3
                \6 4
                \7 5
                \8 6
                \9 7
                \T 8
                \J 9
                \Q 10
                \K 11
                \A 12
                }
               c
               )
              )]
      (let [v (vec s)
            suit-char (first v)
            rank-char (last v)]
        {:suit (suit suit-char) :rank (rank rank-char)})))

(defn get-suits
  [cards]
  (map #(:suit %) cards))

(defn same-suit
  [cards]
  (= 1 (count (set (get-suits cards)))))

(defn get-ranks
  [cards]
  (map #(:rank %) cards))

(defn get-cards
  [hand]
  (map card-suit-rank hand))

;; ----------------------------------------------------------------------------------------------------

(defn straight-flush
  "Straight flush: All cards in the same suit, and in sequence "
  [cards]
  (if (same-suit cards)
      (let [ranks (sort (get-ranks cards))
            cnt (count ranks)]
        (if (and (= cnt 5) (= 4 (- (nth ranks (- cnt 1)) (nth ranks 0))))
          true
          false
          ))))

(defn four-of-a-kind
  "Four of a kind: Four of the cards have the same rank"
  [cards]
  (let [count-map (frequencies (map #(:rank %) cards))]
    ;; count-map contains a value of 4
    (not (empty? (filter (fn [[v c]] (= c 4)) count-map)))
    ))

(defn full-house
  "Full House: Three cards of one rank, the other two of another rank"
  [cards]
  (let [count-map (frequencies (map #(:rank %) cards))]
    ;; count-map contains a value of 3 and 2
    (not (empty? (filter (fn [[v c]] (and (= c 2) (= c 3))) count-map)))))

(defn flush
  "Flush: All cards in the same suit"
  [cards]
  (same-suit cards))

;; @see http://stackoverflow.com/a/3249649
(defn seq-contains?
  "Determine whether a sequence contains a given item"
  [sequence item]
  (if (empty? sequence)
    false
    (reduce #(or %1 %2) (map #(= %1 item) sequence))))

(defn fix-up-ace
  [ranks]
  (if (and (seq-contains? ranks 12) (seq-contains? ranks 2))
    (cons -1 (remove (fn [x] (= 12 x)) ranks))
    ranks
  ))

(defn increasing
  [xs]
  (let [s (sort xs)]
      (loop [s s]
        (if (and (seq s) (= 1 (count s)))
          true
          (let [a (first s)
                b (second s)]
            (if (not= (inc a) b)
              false
              (recur (next s))))))
))

(defn straight
  "Straight: All cards in sequence (aces can be high or low, but not both at once"
  [cards]
  (let [ranks (fix-up-ace (sort (get-ranks cards)))
        cnt (count ranks)]
      (if (and (= cnt 5) 
               ;; TODO
               ;; area all the numbers sequencial
               ;; (= 4 (- (nth ranks (- cnt 1)) (nth ranks 0)))
               (increasing ranks)
               )        
        true
        false
        )
    ))

(defn three-of-a-kind
  "Three of a kind: Three of the cards have the same rank"
  [cards]
  (let [count-map (frequencies (map #(:rank %) cards))]
    (not (empty? (filter (fn [[v c]] (= c 3)) count-map)))
    ))

(defn two-pair
  "Two pair: Two pairs of cards have the same rank"
  [cards]
  (let [count-map (frequencies (map #(:rank %) cards))]
    (= 2 (count (filter (fn [[v c]] (= c 2)) count-map)))))


(defn pair
  "Pair: Two cards have the same rank"
  [cards]
  (let [count-map (frequencies (map #(:rank %) cards))]
    (not (empty? (filter (fn [[v c]] (= c 2)) count-map)))
    )
  )

(defn high-card
  "High card: None of the above conditions are met"
  [cards]
  true
  )

(defn prob178
  [coll]
  (let [cards (get-cards coll)]
    (cond
      (straight-flush cards) :straight-flush
      (four-of-a-kind cards) :four-of-a-kind
      (full-house cards) :full-house
      (flush cards) :flush
      (straight cards) :straight
      (three-of-a-kind cards) :three-of-a-kind
      (two-pair cards) :two-pair
      (pair cards) :pair
      :else :high-card
      ))
  )

(defn run-examples []
  (= :high-card (prob178 ["HA" "D2" "H3" "C9" "DJ"]))
  (= :pair (prob178 ["HA" "HQ" "SJ" "DA" "HT"]))
  (= :two-pair (prob178 ["HA" "DA" "HQ" "SQ" "HT"]))
  (= :three-of-a-kind (prob178 ["HA" "DA" "CA" "HJ" "HT"]))
  (= :straight (prob178 ["HA" "DK" "HQ" "HJ" "HT"]))
  (= :straight (prob178 ["HA" "H2" "S3" "D4" "C5"]))
  (= :flush (prob178 ["HA" "HK" "H2" "H4" "HT"]))  ;; all the same suit
  (= :full-house (prob178 ["HA" "DA" "CA" "HJ" "DJ"]))
  (= :four-of-a-kind (prob178 ["HA" "DA" "CA" "SA" "DJ"]))  ;; four rank aces
  (= :straight-flush (prob178 ["HA" "HK" "HQ" "HJ" "HT"])))



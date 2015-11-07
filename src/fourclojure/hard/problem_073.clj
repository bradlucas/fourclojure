(ns fourclojure.hard.hard.problem-073)


;; Analyze a Tic-Tac-Toe Board
;; #73
(def board [[:x :e :o]
            [:x :e :e]
            [:x :e :o]])

(defn row [m n] (nth m n))

(defn rows [m]
  (map #(row m %) (range 3)))

(defn col [m n] (vec (map #(nth % n) m)))

(defn cols [m]
  (map #(col m %) (range 3)))

(defn diags [m] 
  (list [(last (col m 0)) (second (col m 1)) (first (col m 2))]
        [(first (col m 0)) (second (col m 1)) (last (col m 2))]))

(defn triples [m]
  (concat (rows m) (cols m) (diags m)))
           
(defn all-same? [s]
  (let [v (first s)]
    (if (every? (fn [x] (= x v)) s)
      v
      nil)))

(defn prob73 [m]
  ;; return first path that contains all :x or :o
  ;; if none return nil
  (let [tups (triples m)]
    (if (filter #(= [:x :x :x] %) tups)
      :x
      (if (filter #(= [:o :o :o] %) tups)
      :o
      nil
        )
      )
    )
)

(= :x (prob73 board))


(defn prob73 [m]
  ;; return first path that contains all :x or :o
  ;; if none return nil
  (letfn [(triples [m]
            (concat (rows m) (cols m) (diags m)))
          (rows [m]
            (map #(row m %) (range 3)))
          (row [m n] (nth m n))
          (cols [m]
            (map #(col m %) (range 3)))
          (col [m n] (vec (map #(nth % n) m)))
          (diags [m] 
            (list [(last (col m 0)) (second (col m 1)) (first (col m 2))]
                  [(first (col m 0)) (second (col m 1)) (last (col m 2))]))]
  (let [tups (triples m)]
    (print tups)
    (print (count (filter #(= [:x :x :x] %) tups)))
    (if (> (count (filter #(= [:x :x :x] %) tups)) 0)
      :x
      (if (> (count (filter #(= [:o :o :o] %) tups)) 0)
        :o
        nil
        )))))


(= nil (prob73 [[:e :e :e]
                [:e :e :e]
                [:e :e :e]]))

(= :x (prob73 [[:x :e :o]
               [:x :e :e]
               [:x :e :o]]))

(= :o (prob73 [[:e :x :e]
               [:o :o :o]
               [:x :e :x]]))

(= nil (prob73 [[:x :e :o]
            [:x :x :e]
            [:o :x :o]]))

(= :x (prob73 [[:x :e :e]
           [:o :x :e]
           [:o :e :x]]))

(= :o (prob73 [[:x :e :o]
           [:x :o :e]
           [:o :e :x]]))

(= nil (prob73 [[:x :o :x]
            [:x :o :x]
            [:o :x :o]]))





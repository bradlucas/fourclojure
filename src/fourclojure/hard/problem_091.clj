(ns fourclojure.hard.problem-091)

;; Examples
(def g0 #{[1 2] [2 3] [3 1]
          [4 5] [5 6] [6 4] [3 4]})

(def g1  #{[1 2] [2 3] [3 1] 
           [3 4]
           [4 5] [5 6] [6 4]})

(def a0  #{[:a :b] [:b :c] [:c :d]
           [:x :y] [:d :a] [:b :e] [:x :a]})

(def a1  #{
           [:b :e]
           [:b :c] 

           [:x :a]
           
           [:a :b] 
           [:c :d]
           [:d :a] 

           [:x :y] 

})


;; rearrage nodes so there is a path from any two nodes
;; this means you order them in a connected way so that 
;; [a b] [b c] [c d] ... such that all nodes are in the path

;; ----------------------------------------------------------------------------------------------------

(defn is-path
  "Is there a path from node a to node b
  (path [1 2] [2 3]) -> true
  (path [1 2] [3 2]) -> true"
  [[a b] [c d]]
  (or (= b c)
      (= b d)
      (= a c)
      (= a d)))

(defn remover
  "Return [elem [xs without elm]]"
  [elm xs]
  [elm (filter #(not= elm %) xs)])

(defn combo-splitter
  [xs]
  ;; split all combinations
  ;; given [a b c d]
  ;; return
  ;; [a [b c d]] [b [a c d] [c [a b d]] [d [a b c]]
  (map #(remover % xs) xs))

(defn filter-next-paths
  [node xs]
  (filter (fn [[n r]] (or (nil? node) (is-path node n))) (combo-splitter xs)))

(defn prob91Ex
  "For a given set of nodes return true if they are connected"
  [path xs]
  (if (empty? xs)
    path
    ;; for each item in xs
    ;; if is-path (last path) (first xs)
    ;; call foo (add-path path node) (remove node xs)
    (let [node (last path)
          paths (filter-next-paths node xs)]
      ;;(if (not (empty? paths))
        ;; (dorun (map (fn [[n r]] (println n " - " r)) paths))
        (map (fn [[ a res ]] 
               (prob91Ex (conj path a) res))
             paths)
        )
    ))


(defn prob91 [s] (pos? (count (flatten (prob91Ex [] s)))))

;; NOTES
;; take the first node and then recurse on the rest
;;
;; [a b c d]
;; [a [b c d]]                            [b [a c d] [c [a b d]] [d [a b c]]
;;
;; [a b [c d]]              [a c [b c]] [a d [b c]] 
;;
;; [a b c [d]] [a b d [c]]  
;;
;; [a b c d] [a b d c]
;;
;; call the function 



;; ----------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------

(defn problem91
  [s]
  (letfn [;; Is there a path from node a to node b
          ;; (path [1 2] [2 3]) -> true
          ;; (path [1 2] [3 2]) -> true
          (is-path [[a b] [c d]]
            (or (= b c)
                (= b d)
                (= a c)
                (= a d)))

          ;; Return [elem [xs without elm]]
          (remover [elm xs]
            [elm (filter #(not= elm %) xs)])
          
          ;; split all combinations
          ;; given [a b c d]
          ;; return
          ;; [a [b c d]] [b [a c d] [c [a b d]] [d [a b c]]
          (combo-splitter [xs]
            (map #(remover % xs) xs))

          (filter-next-paths
            [node xs]
            (filter (fn [[n r]] (or (nil? node) (is-path node n))) (combo-splitter xs)))

          ;; For a given set of nodes return true if they are connected
          (problem91Ex [path xs]
            (if (empty? xs)
              path
              ;; for each item in xs
              ;; if is-path (last path) (first xs)
              ;; call foo (add-path path node) (remove node xs)
              (let [node (last path)
                    paths (filter-next-paths node xs)]
                    (map (fn [[ a res ]] (problem91Ex (conj path a) res)) paths))))]
    (pos? (count (flatten (problem91Ex [] s))))))




(= true (problem91 #{[:a :a]}))
(= true (problem91 #{[:a :b]}))
(= false (problem91 #{[1 2] [2 3] [3 1]
                   [4 5] [5 6] [6 4]}))
(= true (problem91 #{[1 2] [2 3] [3 1]
              [4 5] [5 6] [6 4] [3 4]}))
(= false (problem91 #{[:a :b] [:b :c] [:c :d]
               [:x :y] [:d :a] [:b :e]}))
(= true (problem91 #{[:a :b] [:b :c] [:c :d] 
                  [:x :y] [:d :a] [:b :e] [:x :a]}))

    

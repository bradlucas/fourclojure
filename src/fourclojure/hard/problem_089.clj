(ns fourclojure.hard.problem-089)

;; Eulerian path 
;; In graph theory, a Eulerian trail (or Eulerian path) is a trail in
;; a graph which visits every edge exactly once.

;; we are tracking edges so a node can be visited multiple times
;; input tuples are edges
;; 
;; @see https://gist.githubusercontent.com/raw/4008944/9c0577566b9a87e8db03332bf71b6d0b1140748e/4clojure.clj
;; A graph has an Eulerian path if it's connected and at most have two nodes with odd degree

;; https://en.wikipedia.org/wiki/Eulerian_path


(defn problem89
  [graph]
  (letfn [(process [graph]
            (reduce
             (fn [nodes [e1 e2]]
               (let [c1 (or (nodes e1) 0)
                     c2 (or (nodes e2) 0)]
                 ;; ignore if same
                 (if (= e1 e2)
                   nodes
                   (assoc nodes e1 (inc c1) e2 (inc c2)))))
             {}
             graph
             ))]
    (let [nodes (process graph)]
      (and (not (empty? nodes))
           (let [v (vals nodes)]
             ;; true if all odds counts are <= 2
             (>= 2 (count (filter odd? v)))
             ))
      )))

(= true (problem89 [[:a :b]]))
(= false (problem89 [[:a :a] [:b :b]]))
(= false (problem89 [[:a :b] [:a :b] [:a :c] [:c :a] [:a :d] [:b :d] [:c :d]]))
(= true (problem89 [[1 2] [2 3] [3 4] [4 1]]))
(= true (problem89 [[:a :b] [:a :c] [:c :b] [:a :e], [:b :e] [:a :d] [:b :d] [:c :e], [:d :e] [:c :f] [:d :f]]))
(= false (problem89 [[1 2] [2 3] [2 4] [2 5]]))


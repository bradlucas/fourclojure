(ns fourclojure.hard.problem-106)

;; Given a pair of numbers, the start and end point, find a path
;; between the two using only three possible operations:
;;
;;     double 
;;     halve (odd numbers cannot be halved) 
;;     add 2
;;
;; Find the shortest path through the "maze". Because there are multiple
;; shortest paths, you must return the length of the shortest path, not
;; the path itself.

(defn choices
  [path]
  (let [head (first path)]
    (for [op [* / +]
          :let [next (op head 2)]
          :when (integer? next)]
      ;; put at the head
      (cons next path))))   

(defn search
  [path]
  (mapcat choices path))   ;; map choices path and concat results

(defn prob106
  [start end]
  (let [goal (fn [p] (= end (first p)))]
    (count         ;; length of the path is what we want
     (ffirst        ;; first of the first
      (filter seq   ;; remove empty results
              ;; call search looking for results that reach the goal   
              (map #(filter goal %) (iterate search [[start]]))
              )
      )
     )  
    )
  )


(defn prob106
  [start end]
  (let [choices (fn [path]
                  (let [head (first path)]
                    (for [op [* / +]
                          :let [next (op head 2)]
                          :when (integer? next)]
                      ;; put at the head
                      (cons next path))))

        search (fn [path] (mapcat choices path))

        goal (fn [p] (= end (first p)))]
    (count         ;; length of the path is what we want
     (ffirst        ;; first of the first
      (filter seq   ;; remove empty results
              ;; call search looking for results that reach the goal   
              (map #(filter goal %) (iterate search [[start]]))
              )))))


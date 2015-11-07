(ns fourclojure.medium.problem-131)

;; Sum Some Set Subsets
;; https://www.4clojure.com/problem/131
;; 
;; Given a variable number of sets of integers, create a function which
;; returns true iff all of the sets have a non-empty subset with an
;; equivalent summation.


;; http://stackoverflow.com/a/20613311
(defn subsets [s]
  (if (seq s)
    (let [f (first s), srs (subsets (disj s f))]
      (concat srs (map #(conj % f) srs)))
    (list #{})))

(defn add-contents
  "For a given set return the result of summing it's contents"
  [s]
  (reduce + s))

(defn add-set-members
  "For sequences of sets add each's contents and add the results to a set"
  [xs]
  (into #{} (map add-contents xs)))

(defn prob131
  [& sets]
  ;; find all the subsets in each set and sum the values
  ;; find the intersection of all the subset sums
  ;; if there is a common sum then return true
  ;;(map #(map #(reduce + (map identity %)) (vec (subsets %))) sets)
  (pos? (count (apply clojure.set/intersection (map #(add-set-members %) (map #(filter not-empty ( subsets %)) sets))))))



(= true (prob131 #{-1 1 99} 
                 #{-2 2 888}
                 #{-3 3 7777})) ; ex. all sets have a subset which sums to zero


(= false (prob131 #{1}
                  #{2}
                  #{3}
                  #{4}))



;; http://stackoverflow.com/a/20613311
(defn subsets [s]
  (if (seq s)
    (let [f (first s), srs (subsets (disj s f))]
      (concat srs (map #(conj % f) srs)))
    (list #{})))

(defn add-contents
  "For a given set return the result of summing it's contents"
  [s]
  (reduce + s))

(defn add-set-members
  "For sequences of sets add each's contents and add the results to a set"
  [xs]
  (into #{} (map add-contents xs)))


;; 09-28-2015 Submitted
(defn prob131
  [& sets]
  (letfn [(subsets [s]
            (if (seq s)
              (let [f (first s), srs (subsets (disj s f))]
                (concat srs (map #(conj % f) srs)))
              (list #{})))
          (add-contents [s]
            (reduce + s))
          (add-set-members [xs]
            (into #{} (map add-contents xs)))]
    (pos? (count (apply clojure.set/intersection (map #(add-set-members %) (map #(filter not-empty ( subsets %)) sets))))))
  )


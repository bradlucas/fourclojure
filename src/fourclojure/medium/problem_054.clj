(ns fourclojure.medium.problem-054)

;; Partition a Sequence
;; https://www.4clojure.com/problem/54
;; (defn prob54 [n s]
;;   (loop [n n
;;          s s
;;          acc ()]
;;     (if (< (count s) n)
;;       (reverse  acc)
;;       (recur n (drop n s) (conj acc (take n s))))))

(defn prob54 [n s]
  (loop [col s
         acc ()]
    (if (< (count col) n)
      (reverse  acc)
      (recur (drop n col) (conj acc (take n col))))))


(ns fourclojure.medium.problem-103)

;; Generating k-combinations
;; https://www.4clojure.com/problem/103

;; given a sequence of n elements
;; generate all possible sets consiting of k distinct elements taken from S

;; http://pastebin.com/YPn6hsU0
;; https://github.com/brotherb/4clojure/blob/master/103.clj

;; https://cascade.ceas.uc.edu/course/16/public/blog/post/32
;Generate all n choose k combinations from a set of n items
;by concatenating all those that include the first item
;with all those that do not.            
(defn k-comb [ k items ]
  (cond
     (= k 0) '(())
     (empty? items) '()
      :else 
          (concat 
               (map (fn [it] (cons (first items) it)) (k-comb (dec k) (rest items)))
                (k-comb k (rest items)))))


(defn prob103
  [num seq]
  (set (map set   
            (cond
              (= num 0) '(())
              (empty? seq) '()
              :else (concat 
                     (map (fn [it] (cons (first seq) it)) (prob103 (dec num) (rest seq)))
                     (prob103 num (rest seq)))))))


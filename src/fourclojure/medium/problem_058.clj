(ns fourclojure.medium.problem-058)

;; Function Composition 
;; #58
;; apply params functions from right to left
(defn prob58 [& funcs]
  (fn [& args]
    (first 
     (reduce 
      #(vector (apply %2 %1))
      args 
      (reverse funcs)))
    ))

;; (defn prob58 [& funcs]
;;   (fn [& args]
;;     (let [[f & fs] (reverse funcs)]
;;       (reduce #(%2 %1) (apply f args) fs))))


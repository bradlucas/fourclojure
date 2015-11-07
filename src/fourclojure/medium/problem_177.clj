(ns fourclojure.medium.problem-177)


;; Balancing Brackets
;; https://www.4clojure.com/problem/177

;; for each open push on stack and recur with rest
;; for each close verify that top of stack has it's open remove both


;; use '() as stack so conj pushes on the top
(defn prob177 
  ([s] (prob177 s '()))
  ([s stack]
   (let [is-open? #(#{\{ \( \[} %)
         is-close? #(#{\} \) \]} %)
         matches? (fn [open close]
                    (cond 
                      (and (= open \{) (= close \})) true
                      (and (= open \() (= close \))) true
                      (and (= open \[) (= close \])) true
                      :else false))]
     (if-let [c (first s)]
       (cond
         (is-open? c) (recur (rest s) (conj stack c))
         (is-close? c) (when (matches? (first stack) c)
                         (recur (rest s) (rest stack)))
         :else (recur (rest s) stack)
         )
       (empty? stack)))))




(ns fourclojure.medium.problem-148)

;; The Big Divide
;; https://www.4clojure.com/problem/148

;; sum of all natural numbers under n which are dividable by a or b
;; [10 3 5] => (+ 3 5 6 9) == 23

;; a and b are coprimes;; https://en.wikipedia.org/wiki/Coprime_integers

;; ------------------------------------------------------------------------
;; https://groups.google.com/forum/#!topic/4clojure/21p8zcSjopU
(defn prob148 
  [n a b] 
  (reduce + (clojure.set/union (set (range 0 n a))
                               (set (range 0 n b)))))
;; ------------------------------------------------------------------------

;; ------------------------------------------------------------------------
;; https://github.com/khotyn/4clojure-answer/blob/master/148-the-big-divide.clj
;; ------------------------------------------------------------------------

;; ------------------------------------------------------------------------
;; https://www.reddit.com/r/Clojure/comments/2qbzai/clojure_and_project_euler/cn6p5xo
(defn big-divide [n a b]
  (letfn [(n-sum [x limit]
        (let [n-val (quot limit x)]
          (if (pos? n-val)
            (-> (inc n-val) (*' n-val) (/ 2) (*' x) bigint)
            0)))]
(let [limit (dec n)]
  (-> (+' (n-sum a limit) (n-sum b limit))
      (-'  (n-sum (*' a b) limit))))))
;; ------------------------------------------------------------------------

;; ------------------------------------------------------------------------
;; https://en.wikipedia.org/wiki/Arithmetic_progression
;; Sum of n terms of an sequence with n starting 
;; n is the number of terms
;; a is the first term
;; b is the last term
;; NOTE: you need to know the number of terms (n) and the last term
;; (sum-arithmetic-sequence 334 0 999) => 166833
;; (reduce + (range 0 1000 3)) => 166833
(defn sum-arithmetic-sequence
  [n a b]
  (* (/ n 2) (+ a b)))
;; ------------------------------------------------------------------------


(defn term-count 
  "Return the number of items in the range from 0 to bound incremented by v
  Sound equal (count (range 0 bound v))"
  [bound v] 
  (+ (quot bound v) (if (> (rem bound v) 0) 1 0)))

(defn last-term 
  "The last term an in the sequence from a1 to (n-1)d
  where n is the number of terms in the progression (term-count n v)
  (last-term (term-count n d) d)
  "
  [cnt d] 
  (* (dec cnt) d))

(defn sum-terms 
  "The sum of a artithmetic squence is n/2 * (a1 + an)
  n is the numer of terms and an is the last term with d being the interval
  (sum-terms (term-count n d))
  "
  [bound d] 
  (let [cnt (term-count bound d)
        last (last-term cnt d)
        ]
    (println "cnt" cnt)
    (println "last" last)
    (*' (/ cnt 2) last)))
      
(defn prob148 
  [n a b] 
  (if (and (> n a) (> n b))
    (let [fifteen (if (> n (* a b)) (sum-terms n (* a b)) 0)]
      (- (+ (sum-terms n a) (sum-terms n b)) fifteen)
      )
    0))

(defn prob148 
  [n a b] 
  (letfn [(term-count [bound v] (+ (quot bound v) (if (> (rem bound v) 0) 1 0)))
          (last-term [cnt d] (* (dec cnt) d))
          (sum-terms [bound d] (let [cnt (term-count bound d)
                                     last (last-term cnt d)]
                                 (*' (/ cnt 2) last)))]
    (if (and (> n a) (> n b))
      (let [fifteen (if (> n (* a b)) (sum-terms n (* a b)) 0)]
        (- (+ (sum-terms n a) (sum-terms n b)) fifteen))
      0)))





(ns fourclojure.medium.problem-116)


;; Prime Sandwich
;; https://www.4clojure.com/problem/116

(defn get-nth-pos-of-n
  [n]
  ;; in the sequence of primes, what position is n
  ;; recur of the list of primes until you find n or you go past it (n is not a prime)
  (loop [pos 1]
    (print pos)
    (let [v (last (prob67 pos))]
    (if (= v n)
      pos
      (if (> v n)
        false
        (recur (+ pos 1)))))))

(defn meanmiddle
  "Return true if b is the mean of a and c"
  [a b c]
  (= b (/ (+ a c) 2)))

(defn get-nth-prime
  [pos]
  ;; return the nth prime
  (last (prob67 pos)))

(defn prob116 
  [n]
  (let [pos (get-nth-pos-of-n n)]
    (if pos
      (meanmiddle (get-nth-prime (- pos 1)) n (get-nth-prime (+ pos 1))) 
      false)))

(defn prob116 
  [n]
  (letfn [(prob67 [n]
            (take n
                  (cons 2 (filter 
                           (fn [x] ;; is prime
                             (empty? (filter #(= 0 (mod x %)) (range 2 x)))
                             )
                           (iterate #(+ 2 %) 3)))))
          (get-nth-pos-of-n [n]
            ;; in the sequence of primes, what position is n
            ;; recur of the list of primes until you find n or you go past it (n is not a prime)
            (loop [pos 1]
              (print pos)
              (let [v (last (prob67 pos))]
                (if (= v n)
                  pos
                  (if (> v n)
                    false
                    (recur (+ pos 1)))))))
          (get-nth-prime [pos]
            ;; return the nth prime
            (last (prob67 pos)))
          (meanmiddle [a b c]
            (= b (/ (+ a c) 2)))
          ]
    (let [pos (get-nth-pos-of-n n)]
      (if pos
        (meanmiddle (get-nth-prime (- pos 1)) n (get-nth-prime (+ pos 1))) 
        false))))

(defn prob116
  [n]
  (let [prime? (fn [x]
                 (if (or (= x 1) (= x 2))
                   true
                   (empty? (filter #(= 0 (mod x %)) (range 2 x)))))
        primes (filter prime? (drop 1 (range)))
        meanmiddle (fn [a b c] (= b (/ (+ a c) 2)))]
    (if (or (not (prime? n)) (<= n 2))
      false
      (let [prev (last (take-while #(< % n) primes))
            next (first (drop-while #(<= % n) primes))]
        (meanmiddle prev n next)))))


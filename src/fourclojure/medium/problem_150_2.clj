
;; See problem_150.clj for final version. 
;; These are notes 

;; 150
;; Palindromic Numbers
 
;; http://www.4clojure.com/problem/150

;; http://stackoverflow.com/questions/19249851/4clojure-palindrome-number-timeout-issue

;; Write a function which takes an integer n, as its only argument, and returns an increasing lazy sequence of all palindromic numbers that are not less than n.

(defn palindrome? [s]
             (let [s (seq (partition 1 (str s)))]
               (= s (reverse s))))


;; faster than previous
(defn palindrome? [^String s]
  (= s (clojure.string/reverse s)))

;; https://groups.google.com/d/msg/clojure/DNdWjsc7av8/_rn0Zzkn5_EJ
 (defn palindrome?
  [n]
  (let [s (str n)
        len (.length s)
        mid (quot len 2)]
    (loop [i 0
           j (dec len)]
      (if (= i mid)
        true
        (when (= (.charAt s i) (.charAt s j))
          (recur (inc i) (dec j))))))) 


;;  rev = 0;
;;  while (num > 0)
;;  {
;;       dig = num % 10;
;;       rev = rev * 10 + dig;
;;       num = num / 10;
;;  }
 
;; If n == rev then num is a palindrome:

(defn palindrome?
  [num]
  (loop [n num
         reversed 0
         ]
    (if (<= n 0)
      (= num reversed)
      (recur (long (/ n 10)) (+ (* 10 reversed) (mod n 10))))))

;; http://rosettacode.org/wiki/Palindrome_detection#Clojure
(defn palindrome? [num]
  (let [s (str num)]
    (loop [i 0
           j (dec (. s length))]
      (cond (>= i j) true
            (= (get s i) (get s j))
            (recur (inc i) (dec j))
          :else false))))

;; OTHERS
;; http://stackoverflow.com/questions/199184/how-do-i-check-if-a-number-is-a-palindrome

;; https://searchcode.com/codesearch/view/67977972/
(def is-palindrom? (fn [x] 
                            (let
                              [strx (str x)
                               size (int (/ (count strx) 2))
                               lft (take size strx)
                               rht (take size (reverse strx))]
                              (= lft rht))))
(def  sub-generator (fn [x]
                      (if (sequential? x)
                        (let [[half is-odd?] x
                              rollover? (apply = (conj half \9))
                              half (seq (str (inc (bigint (apply str half)))))]
                          (if rollover?
                            (if is-odd?
                              [(drop-last half) (not is-odd?)]
                              [half (not is-odd?)])
                            [half is-odd?]))
                        (let [seqx (str x) ; Number as a sequence
                              size (count seqx) ; Number of digits in number
                              half (take (/ size 2) seqx)] ; First half of the number
                          (if (is-palindrom? x)
                            [(take (/ size 2) seqx) (odd? size)]
                            (if (> (bigint (apply str half)) ; First half is larger than second half
                                   (bigint (apply str (take (/ size 2) (reverse seqx)))))
                              [half (odd? size)]
                              [(seq (str (inc (bigint (apply str half))))) (odd? size)]))))))


(defn prob150
  [n]
    (filter palindrome? (iterate inc n)))


;; https://github.com/eigenhombre/probs4clojure/blob/master/test/probs4clojure/core_test.clj

(defn split-number
  [n]
  [(quot (inc n) 2), (quot n 2)])

(defn build 
  [s left right]
  (Long/parseLong (apply str (concat (take left s) (reverse (take right s))))))

(defn next-palindrome
  [n]
  (let [s (str n)
        [left _] (split-number (count s))
        next (inc (Long/parseLong (apply str (take left s))))
        [new-left new-right] (split-number (count (str (inc n))))
        ]
    (build (str next) new-left new-right)))

(defn problem-150
  [n]
  (drop-while #(< % n) (iterate next-palindrome n)))



(defn prob-150
  [n]
  (letfn [(split-number [n] [(quot (inc n) 2), (quot n 2)])
          (build [s left right] (Long/parseLong (apply str (concat (take left s) (reverse (take right s))))))
          (nearest-palindrome [x]
            (let [sx (str x)
                  [k1 k2] (split-number (count sx))]
              (build sx k1 k2)))
          (next-palindrome [n]
            (let [s (str n)
                  [left _] (split-number (count s))
                  next (inc (Long/parseLong (apply str (take left s))))
                  [new-left new-right] (split-number (count (str (inc n))))]
              (build (str next) new-left new-right)))]
    (drop-while #(< % n) (iterate next-palindrome (nearest-palindrome n)))))



(defn prob150-test []
  (and 
   (= (take 26 (prob150 0))
      [0 1 2 3 4 5 6 7 8 9 
       11 22 33 44 55 66 77 88 99 
       101 111 121 131 141 151 161])
   
   (= (take 16 (prob150 162))
      [171 181 191 202 
       212 222 232 242 
       252 262 272 282 
       292 303 313 323])
   
   (= (take 6 (prob150 1234550000))
      [1234554321 1234664321 1234774321 
       1234884321 1234994321 1235005321])
   
   (= (first (prob150 (* 111111111 111111111)))
      (* 111111111 111111111))
   
   (= (set (take 199 (prob150 0)))
      (set (map #(first (prob150 %)) (range 0 10000))))
   
   (= true 
      (apply < (take 6666 (prob150 9999999))))
   
   (= (nth (prob150 0) 10101)
      9102019)
   ))








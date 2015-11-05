(ns fourclojure.medium.problem-150)

;; Palindromic Numbers
;; https://www.4clojure.com/problem/150

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
        [new-left new-right] (split-number (count (str (inc n))))]
    (build (str next) new-left new-right)))

(defn problem-150
  [n]
  (drop-while #(< % n) (iterate next-palindrome n)))



(defn problem-150
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



;; https://github.com/eigenhombre/probs4clojure/blob/master/test/probs4clojure/core_test.clj

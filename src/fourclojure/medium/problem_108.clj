(ns fourclojure.medium.problem-108)

;; Lazy Searching
;; https://www.4clojure.com/problem/108

;; (map (fn [& args] (reduce + args)) [1 2 3] [1 2 3] [1 2 3])

;; [1 2 3] [1 2 3] [1 2 3]

;; http://image.slidesharecdn.com/soligorsk-130123102755-phpapp01/95/isoligorsk-3-2013-48-638.jpg?cb=1358936997

(defn prob108
  [& colls]
  (letfn [(all-same? 
            [& args]
            (let [first-value (first args)]
              (every? (fn [x] (= first-value x)) args)))]
    (map all-same? colls)
    )
  )

;; https://github.com/khotyn/4clojure-answer/blob/master/108-lazy-searching.clj
;; https://gist.github.com/prajwalit/1186723
;; http://pastebin.com/WubAHDmV

(defn max-first-value
  [colls]
  (apply max (map first colls)))

(defn all-first-same
  [cols]
  (apply = (map first cols)))

(defn drop-leading-smaller
  "Remove all leading values in each coll where it is smaller than val"
  [val colls]
  (map (partial drop-while #(< % val)) colls)
  )

(defn prob108
  [& colls]
  (let [max-first-value (max-first-value colls)]
    (if (all-first-same colls)
      max-first-value
      (recur (drop-leading-smaller max-first-value colls)))))

(defn prob108
  [& colls]
  (letfn [(max-first-value
            [colls]
            (apply max (map first colls)))
          (all-first-same
            [cols]
            (apply = (map first cols)))
          (drop-leading-smaller
            [val colls]
            (map (partial drop-while #(< % val)) colls)
            )]
  (let [max-first-value (max-first-value colls)]
    (if (all-first-same colls)
      max-first-value
      (recur (drop-leading-smaller max-first-value colls))))
))


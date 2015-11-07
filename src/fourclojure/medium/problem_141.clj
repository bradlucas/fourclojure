(ns fourclojure.medium.problem-141)

;; Tricky card games
;; https://www.4clojure.com/problem/141

;; {:suit :diamond :rank 10} 
;;
;; :club :diamond :spade :heart

;; [{:suit :spade :rank 2} {:suit :club :rank 10}]
;; [{:suit :heart :rank 6} {:suit :heart :rank 8} {:suit :diamond :rank 10} {:suit :heart :rank 4}]o(

(defn prob141
  [trump]
  (fn [trick]
    ;; if no trump is passed in use the suit of the first item in trick
    (let [suit (if trump trump (:suit (first trick)))]
      ;; filter by suit matching trick then sort by :rank to get highest card
      (last (sort-by :rank (filter #(= suit (:suit %)) trick))))))


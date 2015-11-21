(ns fourclojure.hard.problem-084)

;; @see https://groups.google.com/d/msg/4clojure/Sy0v-XGhwlI/EhauJsju0KQJ

;; 

;; #{["cat" "man"] 
;;   ["man" "snake"] 
;;   ["spider" "cat"]}


;; #{["cat" "man"] 
;;   ["man" "snake"]
;;   ["spider" "cat"] 
;;
;;   ["cat" "snake"]    << cat -> man -> snake
;;   ["spider" "man"]   << spider -> cat -> man
;;   ["spider" "snake"] << spider -> cat -> man -> snake
;;   }



(def a #{["cat" "man"] ["man" "snake"] ["spider" "cat"]})
(def a1 #{["cat" "man"] ["cat" "snake"] ["man" "snake"] ["spider" "cat"] ["spider" "man"] ["spider" "snake"]})

(defn prob81
  [s]
  (letfn [(fnc [elm s]
            (let [nxt (filter #(= (second elm) (first %)) s)]
              (if (seq nxt)
                (do 
                  ;; (println elm nxt)
                  ;; merge elm ->next
                  (let [mrg (map #(vec [(first elm) (second %)]) nxt)]
                    (cons elm (mapcat #(fnc % s) mrg))
                    )
                  )
                [elm]
                )))]
    (set (mapcat #(fnc % s) s))))


(= a1 (prob81 a))


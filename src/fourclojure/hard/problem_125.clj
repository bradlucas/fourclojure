(ns fourclojure.hard.problem-125)

;; @see http://geyeguess.blogspot.com/2012/03/quine-in-lisp-explained.html
;; @see https://www.nyx.net/~gthompso/quine.htm
;; @see http://www.madore.org/~david/computers/quine.html

;; A version based on Lisp solutions
((fn [x] (list x (list (quote quote) x))) (quote (fn [x] (list x (list (quote quote) x)))))

;; ((fn [x] (list x (list (quote quote) x))) (quote (fn [x] (list x (list (quote quote) x)))))
;; ((fn [x] (list x (list (quote quote) x))) (quote (fn [x] (list x (list (quote quote) x)))))

;; Doesn't pass the test in 4Clojure because it is calling str on the function (???)
;; Will need to a string



(
 (fn [s] (str s s)) (quote (fn [s] (str s s)))
)



;; @see https://github.com/khotyn/4clojure-answer/blob/master/125-gus-guinundrum.clj
;; @see https://gist.github.com/anonymous/1256993
;; @see http://pastebin.com/gQv7i60S
;; @see https://gist.github.com/SegFaultAX/3607101 (search for Problem 125)

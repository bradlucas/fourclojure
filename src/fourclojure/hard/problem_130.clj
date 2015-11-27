(ns fourclojure.hard.problem-130)

;; Tree reparenting

;; ----------------------------------------------------------------------------------------------------
;; 1. Find requested node to set as new tree parent
;; 2. Add parent of node as it's right most child branch with it's other children in place
;; 3. If node's parent's parent (grandparent) exists add it as the right most child of the parent

;; ----------------------------------------------------------------------------------------------------
;; 2. Example - b becomes a child of c
(def b  '(b 
          (c (d) (e)) 
          (f (g) (h))))
(def b' '(c 
          (d) (e) (b 
                   (f (g) (h))))) 

;; ----------------------------------------------------------------------------------------------------
;; 3. Example 0 a becomes a child of b
;; if grandparent then push down and to the right of the parent
;; in other words, if grandparent, make it a child of the paren

(def t   '(a 
           (b (c (d) (e)) 
              (f (g) (h))) 
           (i (j (k) (l)) (m (n) (o)))))


(def t1  '(b (c (d) (e)) 
            (f (g) (h)) 
            (a 
             (i (j (k) (l)) (m (n) (o))))))

(def t2  '(c (d) (e) (b 
                      (f (g) (h)) 
                      (a 
                       (i (j (k) (l)) (m (n) (o)))))))


(defn find-node
  [tree node]
  ;; return node (tree)
  ;; (a (b (c (d) (e)) (f (g) (h))) (i (j (k) (l)) (m (n) (o))), c
  ;; (c (d) (e))
  (loop [head (first tree)
         children (rest tree)]
    (if (= head node)
      tree
      (mapcat #(find-node % node) children))))

(defn add-child
  [tree node]
  ;; add node(tree) as the right branch of tree
  ;; (add-child '(c (d) (e))  '(b (f (g) (h)) (a (i (j (k) (l)) (m (n) (o))))))
  ;; (c (d) (e) (b (f (g) (h)) (a (i (j (k) (l)) (m (n) (o))))))
  (concat tree (list node)))

(defn find-parent
  [tree node]
  ;; return node's parent (tree)
  ;; (a (b (c (d) (e)) (f (g) (h))) (i (j (k) (l)) (m (n) (o))), c
  (loop [head (first tree)
         children (rest tree)]
    ;; has child with node
    (if (some #(= node %) (map first children))
      tree
      (mapcat #(find-parent % node) children))))


(defn remove-sub-tree
  [tree node]
  ;; return tree without sub-tree removed
  (loop [head (first tree)
         children (rest tree)]
    (apply list head (filter #(not= node %) children))))


(defn push-parent-down-right
  [tree node]
  ;; (b 
  ;;   (c (d) (e)) 
  ;;   (f (g) (h))))
  (let [
        ;; find node tree
        ;;   (c (d) (e)) 
        node-tree (find-node tree node)

        ;; find parent of node
        ;; (b 
        parent-tree (find-parent tree node)

        ;; remove the sub-tree node from the parent
        ;; (b 
        ;;   (f (g) (h))))
        parent-tree' (remove-sub-tree parent-tree node-tree)

        ;; find the parent of parent
        parent-parent-tree (find-parent tree (first parent-tree))

        ;; remove parent tree from parent of parent
        parent-parent-tree' (remove-sub-tree parent-parent-tree parent-tree)]
    ;; (println node-tree)
    ;; (println parent-tree)
    ;; (println parent-tree')
    ;; (println parent-parent-tree)
    (println parent-parent-tree')

    (if (nil? (first parent-tree'))
      node-tree
      ;; push parent down and to the right (make it additional right child branch
      ;; (c 
      ;;    (d) (e)
      ;;           (b 
      ;;              (f (g) q(h)))
      ;; 
      (add-child node-tree 
                 (if (seq parent-parent-tree)
                   (add-child parent-tree' parent-parent-tree')   ;; make the parent's parent a child of parent
                   parent-tree')
                 ))))

(defn prob130
  [node tree]
  (push-parent-down-right tree node))


;; ----------------------------------------------------------------------------------------------------
;; Testing

(= '(n)
   (prob130 'n '(n)))

(= '(a (t (e)))
   (prob130 'a '(t (e) (a))))

(= '(e (t (a)))
   (prob130 'e '(a (t (e)))))

(= '(a (b (c)))
   (prob130 'a '(c (b (a)))))

(= '(d 
      (b
        (c)
        (e)
        (a 
          (f 
            (g) 
            (h)))))

  (prob130 'd '(a
            (b 
              (c) 
              (d) 
              (e))
            (f 
              (g)
              (h)))))

(= '(c 
      (d) 
      (e) 
      (b
        (f 
          (g) 
          (h))
        (a
          (i
          (j
            (k)
            (l))
          (m
            (n)
            (o))))))
   (prob130 'c '(a
             (b
               (c
                 (d)
                 (e))
               (f
                 (g)
                 (h)))
             (i
               (j
                 (k)
                 (l))
               (m
                 (n)
                 (o))))))

 




;; ----------------------------------------------------------------------------------------------------

(defn prob130
  [node tree]
  (letfn [(find-node
            [tree node]
            ;; return node (tree)
            ;; (a (b (c (d) (e)) (f (g) (h))) (i (j (k) (l)) (m (n) (o))), c
            ;; (c (d) (e))
            (loop [head (first tree)
                   children (rest tree)]
              (if (= head node)
                tree
                (mapcat #(find-node % node) children))))
          
          (add-child
            [tree node]
            ;; add node(tree) as the right branch of tree
            ;; (add-child '(c (d) (e))  '(b (f (g) (h)) (a (i (j (k) (l)) (m (n) (o))))))
            ;; (c (d) (e) (b (f (g) (h)) (a (i (j (k) (l)) (m (n) (o))))))
            (concat tree (list node)))
          
          (find-parent
            [tree node]
            ;; return node's parent (tree)
            ;; (a (b (c (d) (e)) (f (g) (h))) (i (j (k) (l)) (m (n) (o))), c
            (loop [head (first tree)
                   children (rest tree)]
              ;; has child with node
              (if (some #(= node %) (map first children))
                tree
                (mapcat #(find-parent % node) children))))
          
          
          (remove-sub-tree
            [tree node]
            ;; return tree without sub-tree removed
            (loop [head (first tree)
                   children (rest tree)]
              (apply list head (filter #(not= node %) children))))
          
          
          (push-parent-down-right
            [tree node]
            ;; (b 
            ;;   (c (d) (e)) 
            ;;   (f (g) (h))))
            (let [
                  ;; find node tree
                  ;;   (c (d) (e)) 
                  node-tree (find-node tree node)
                  
                  ;; find parent of node
                  ;; (b 
                  parent-tree (find-parent tree node)
                  
                  ;; remove the sub-tree node from the parent
                  ;; (b 
                  ;;   (f (g) (h))))
                  parent-tree' (remove-sub-tree parent-tree node-tree)
                  
                  ;; find the parent of parent
                  parent-parent-tree (find-parent tree (first parent-tree))
                  
                  ;; remove parent tree from parent of parent
                  parent-parent-tree' (remove-sub-tree parent-parent-tree parent-tree)]
              ;; (println node-tree)
              ;; (println parent-tree)
              ;; (println parent-tree')
              ;; (println parent-parent-tree)
              (println parent-parent-tree')
              
              (if (nil? (first parent-tree'))
                node-tree
                ;; push parent down and to the right (make it additional right child branch
                ;; (c 
                ;;    (d) (e)
                ;;           (b 
                ;;              (f (g) q(h)))
                ;; 
                (add-child node-tree 
                           (if (seq parent-parent-tree)
                             (add-child parent-tree' parent-parent-tree')   ;; make the parent's parent a child of parent
                             parent-tree')
                           ))))]
    (push-parent-down-right tree node)))

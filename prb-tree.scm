;; Persistent red-black trees.  We can't use mit-scheme's built-in
;; implementation, because it is done in imperative style and, in
;; particular, maintains upwards pointers which make immutability
;; not feasible.

(declare (usual-integrations))

(define-structure (tree
		   (predicate prb-tree?)
		   (constructor make-tree (root key=? key<?)))
  (root #f)
  (key=? #f read-only #t)
  (key<? #f read-only #t))

(define (make-prb-tree key=? key<?)
  ;; Optimizations to work around compiler that codes known calls to
  ;; these primitives much more efficiently than unknown calls.
  (make-tree (cond ((eq? key=? eq?) (lambda (x y) (eq? x y)))
                   ((eq? key=? fix:=) (lambda (x y) (fix:= x y)))
                   ((eq? key=? flo:=) (lambda (x y) (flo:= x y)))
                   (else key=?))
             (cond ((eq? key<? fix:<) (lambda (x y) (fix:< x y)))
                   ((eq? key<? flo:<) (lambda (x y) (flo:< x y)))
                   (else key<?))))

(define-integrable (guarantee-prb-tree tree procedure)
  (if (not (prb-tree? tree))
      (error:wrong-type-argument tree "persistent red/black tree" procedure)))

(define-structure (node (constructor make-node (color key datum left right))
                        (predicate node?))
  color
  key
  datum
  left
  right)

(define (prb-tree/empty)
  (make-tree #f fix:= fix:<))

(define (prb-tree/lookup tree key if-found if-not-found)
  (guarantee-prb-tree tree 'PRB-TREE/LOOKUP)
  (let ((key=? (tree-key=? tree))
        (key<? (tree-key<? tree)))
      (let loop ((node (tree-root tree)))
        (if (false? node)
          (if-not-found)
          (let ((cur-key (node-key node)))
              (cond ((key<? key cur-key) (loop (node-left node)))
                    ((key=? key cur-key) (if-found (node-datum node)))
                    (else (loop (node-right node)))))))))

(define (prb-tree/insert tree key datum)
  (guarantee-prb-tree tree 'PRB-TREE/INSERT)
  (let ((key=? (tree-key=? tree))
        (key<? (tree-key<? tree)))
    (define (ins tree)
      (if (false? tree)
        (make-node 'RED key datum #f #f)
        (let ((cur-key (node-key tree))
              (color (node-color tree))
              (a (node-left tree))
              (b (node-right tree))
              (v (node-datum tree)))
          (cond ((key<? key cur-key) (balance color cur-key v (ins a) b))
                ((key=? key cur-key) (make-node color cur-key datum a b))
                (else                (balance color cur-key v a (ins b)))
                ))))
    (define (balance color x v a b)
        (or
          (if (eq? color 'BLACK)
            (or
              (if (node? a)
                (if (eq? 'RED (node-color a))
                  (or
                    (let ((la (node-left a)))
                      (if (and (node? la) (eq? 'RED (node-color la)))
                        (make-node 'RED (node-key a) (node-datum a)
                                   (make-node 'BLACK (node-key la) (node-datum la) (node-left la) (node-right la))
                                   (make-node 'BLACK x v (node-right a) b)) #f))
                    (let ((ra (node-right a)))
                      (if (and (node? ra) (eq? 'RED (node-color ra)))
                        (make-node 'RED (node-key ra) (node-datum ra)
                                   (make-node 'BLACK (node-key a) (node-datum a) (node-left a) (node-left ra))
                                   (make-node 'BLACK x v (node-right ra) b)) #f))
                    ) #f) #f)
              (if (node? b)
                (if (eq? 'RED (node-color b))
                  (or
                    (let ((lb (node-left b)))
                      (if (and (node? lb) (eq? 'RED (node-color lb)))
                        (make-node 'RED (node-key lb) (node-datum lb)
                                   (make-node 'BLACK x v a (node-left lb))
                                   (make-node 'BLACK (node-key b) (node-datum b) (node-right lb) (node-right b))) #f))
                    (let ((rb (node-right b)))
                      (if (and (node? rb) (eq? 'RED (node-color rb)))
                        (make-node 'RED (node-key b) (node-datum b)
                                   (make-node 'BLACK x v a (node-left b))
                                   (make-node 'BLACK (node-key rb) (node-datum rb) (node-left rb) (node-right rb))) #f))
                    ) #f) #f)
              ) #f)
          (make-node color x v a b)))
    (define (make-black tree)
      (make-node 'BLACK (node-key tree) (node-datum tree) (node-left tree) (node-right tree)))
    (make-tree (make-black (ins (tree-root tree))) key=? key<?)
    ))



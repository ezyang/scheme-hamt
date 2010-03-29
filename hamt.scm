(declare (usual-integrations))

(define fixnum-max
    (let loop ((n 1))
       (if (fix:fixnum? n)
           (loop (* n 2))
           (- n 1))))

(define (hamt-hash x)
  ;; alternative hash function guaranteed to return a fixnum.  Note
  ;; that we don't use the fixnum space as efficiently as possible;
  ;; in particular no negative numbers are generated
  (eq-hash x)) ;; I hope this gets optimized

(define (popcount x)
  (let loop ((x x) (c 0))
    (if (fix:zero? x)
    c
    (loop (fix:lsh x -1) (if (fix:zero? (fix:and 1 x)) c (fix:1+ c))))))

(define-structure hamt-empty)
(define-structure hamt-bitmap bitmap vector) ;; bitmap vector
(define-structure hamt-leaf key real-key value) ;; key value
(define-structure hamt-collision key alist) ;; key value

(define bits-per-subkey 4)
(define subkey-mask #xF)
(define (mask-index b m) (popcount (fix:and b (fix:-1+ m))))
(define (mask k s) (fix:lsh 1 (subkey k s)))
(define (subkey k s) (fix:and (fix:lsh k (fix:- 0 s)) subkey-mask))

(define (lookup k rk s t sc fc)
  (cond ((hamt-empty? t) (fc))
        ((hamt-leaf? t)
         (if (fix:= (hamt-leaf-key t) k)
           (sc (hamt-leaf-value t))
           (fc)))
        ((hamt-bitmap? t)
         (let ((m (mask k s))
               (b (hamt-bitmap-bitmap t)))
           (if (fix:= (fix:and b m) 0)
             (fc)
             (lookup k rk
                     (fix:+ s bits-per-subkey)
                     (vector-ref (hamt-bitmap-vector t)
                                 (mask-index b m))
                     sc
                     fc))))
        ((hamt-collision? t)
         (let ((r (assq rk (hamt-collision-alist t))))
           (if (false? r)
             (fc)
             (sc (second r)))))
        (else (error "invalid datatype passed to lookup"))))

(define (hamt-lookup rk t success-continuation fail-continuation)
  (lookup (hamt-hash rk) rk 0 t success-continuation fail-continuation))

(define (insert kx rkx s x t)
  (cond ((hamt-empty? t) (make-hamt-leaf kx rkx x))
        ((hamt-leaf? t)
         (let ((ky  (hamt-leaf-key t))
               (rky (hamt-leaf-real-key t))
               (y   (hamt-leaf-value t)))
           (if (fix:= ky kx)
             (if (eq? rky rkx)
               (make-hamt-leaf kx rkx x)
               (insert kx rkx s x (make-hamt-collision kx (list (list rky y)))))
             (insert kx rkx s x (make-hamt-bitmap (mask ky y) (vector t))))
           ))
        ((hamt-bitmap? t)
         (let* ((b (hamt-bitmap-bitmap t))
                (v (hamt-bitmap-vector t))
                (m (mask kx s))
                (i (mask-index b m)))
           (if (fix:= (fix:and b m) 0)
             (let* ((l (make-hamt-leaf kx rkx x))
                    (vp (vector-append (vector-head v i) (vector l) (vector-tail v i)))
                    (bp (fix:or b m)))
               (make-hamt-bitmap bp vp)
               )
             (let* ((st (vector-ref v i))
                    (stp (insert kx rkx (+ s bits-per-subkey) x st))
                    (vp (vector-copy v)))
               (vector-set! vp i stp)
               (make-hamt-bitmap b vp)
               )
             )
           ))
        ((hamt-collision? t)
         (let ((alist (list-copy (hamt-collision-alist t))))
           (del-assq! rkx alist)
           (make-hamt-collision kx (cons (list rkx x) alist))))
        (else (error "invalid datatype passed to insert"))
        )
  )

(define (hamt-insert rk v t) (insert (hamt-hash rk) rk 0 v t))

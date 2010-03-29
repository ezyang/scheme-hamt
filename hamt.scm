(define (popcount x)
  (let loop ((x x) (c 0))
    (if (fix:zero? x)
    c
    (loop (fix:lsh x -1) (if (fix:zero? (fix:and 1 x)) c (+ c 1))))))

(define-structure hamt-empty)
(define-structure hamt-bitmap bitmap vector) ;; bitmap vector
(define-structure hamt-leaf key value) ;; key value

(define subkey-mask (let loop ((n 1))
                         (if (fix:fixnum? n)
                              (loop (* n 2))
                              (- n 1))))
(define (mask-index b m) (popcount (fix:and b (fix:-1+ m))))
(define (mask k s) (fix:lsh 1 (subkey k s)))
(define (subkey k s) (fix:and (fix:lsh k (fix:- 0 s)) ()))
(define bits-per-subkey 5)

(define (hamt-lookup k t success-continuation fail-continuation)
  (lookup k 0 t success-continuation fail-continuation))

(define (lookup k s t sc fc)
  (cond ((hamt-empty? t) (fc))
        ((hamt-leaf? t)
         (if (fix:= (hamt-leaf-key t) k)
           (sc (hamt-leaf-value t))
           (fc)))
        ((hamt-bitmap? t)
         (let ((m (mask k s)))
           (if (fix:= (fix:and b m) 0)
             (fc)
             (lookup k
                     (fix:+ s bits-per-subkey)
                     (vector-ref (hamt-bitmap-vector t)
                                 (mask-index (hamt-bitmap-bitmap t) m))
                     sc
                     fc))))
        (else (error "invalid datatype passed to lookup"))))

(define (hamt-insert k v t) (insert k 0 v t))

(define (insert kx s x t)
  (cond ((hamt-empty? t) (make-hamt-leaf kx x))
        ((hamt-leaf? t)
         (let ((ky (hamt-leaf-key t))
               (y  (hamt-leaf-value t)))
           (if (fix:= ky y)
             (make-leaf kx x)
             (insert kx s x (make-hamt-bitmap (mask ky y) (vector t))))
           ))
        ((hamt-bitmap? t)
         (let ((b (hamt-bitmap-bitmap t))
               (v (hamt-bitmap-vector t))
               (m (mask kx s))
               (i (maskIndex b m)))
           (if (fix:= (fix:and b m) 0)
             (let ((l (make-hamt-leaf kx x))
                   (vp (vector-append (vector-head v i) (vector t) (vector-tail v i)))
                   (bp (fix:or b m)))
               ;; add support for full later
               (hamt-bitmap bp vp)
               )
             (let* ((st (vector-ref v i))
                    (stp (insert kx (+ s bits-per-subkey) x st))
                    (vp (vector-copy v)))
               (vector-set! vp i stp)
               (make-hamt-bitmap b vp)
               )
             )
           ))
        (else (error "invalid datatype passed to insert"))
        )
  )

(pp (hamt-lookup 23 (hamt-insert 23 "asdf" (make-hamt-empty)) (lambda (x) x) (lambda () #f)))
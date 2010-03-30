(declare (usual-integrations))

;; This current implementation only supports eq? hash-tables.
;; This should be made more flexible with some sort of template.

(define-integrable (hamt-hash x)
  ;; alternative hash function guaranteed to return a fixnum.  Note
  ;; that we don't use the fixnum space as efficiently as possible;
  ;; in particular no negative numbers are generated
  (eq-hash x))

;; liberal guess at 25-bits, we have to truncate to keep everything
;; fixnum
(define-integrable m1 #x1555555)
(define-integrable m2 #x1333333)
(define-integrable m4 #x10F0F0F)
(define (popcount x)
  (let* ((x (fix:- x (fix:and (fix:lsh x -1) m1)))
         (x (fix:+ (fix:and x m2) (fix:and (fix:lsh x -2) m2)))
         (x (fix:and (fix:+ x (fix:lsh x -4)) m4))
         (x (fix:+ x (fix:lsh x -8))))
    (fix:and (fix:+ x (fix:lsh x -16)) #x3f)))

(define-structure hamt-empty)
(define-structure hamt-bitmap bitmap vector) ;; bitmap vector
(define-structure hamt-leaf key real-key value) ;; key value
(define-structure hamt-collision key alist) ;; key value

(define (hamt? t) (or (hamt-empty? t) (hamt-bitmap? t) (hamt-leaf? t) (hamt-collision? t)))

(define-integrable hamt/bits-per-subkey 4)
(define-integrable hamt/subkey-mask #xF)
(define (hamt/mask-index b m) (popcount (fix:and b (fix:-1+ m))))
(define (hamt/mask k s) (fix:lsh 1 (hamt/subkey k s)))
(define-integrable (hamt/subkey k s) (fix:and (fix:lsh k (fix:- 0 s)) hamt/subkey-mask))

(define make-hamt make-hamt-empty)

(define (%lookup k rk s t sc fc)
  (cond
        ((hamt-bitmap? t)
         (let ((m (hamt/mask k s))
               (b (hamt-bitmap-bitmap t)))
           (if (fix:= (fix:and b m) 0)
             (fc)
             (%lookup k rk
                     (fix:+ s hamt/bits-per-subkey)
                     (vector-ref (hamt-bitmap-vector t)
                                 (hamt/mask-index b m))
                     sc
                     fc))))
        ((hamt-leaf? t)
         (if (and (fix:= (hamt-leaf-key t) k) (eq? (hamt-leaf-real-key t) rk))
           (sc (hamt-leaf-value t))
           (fc)))
        ((hamt-empty? t) (fc))
        ((hamt-collision? t)
         (let ((r (assq rk (hamt-collision-alist t))))
           (if (false? r)
             (fc)
             (sc (second r)))))
        (else (error "invalid datatype passed to lookup"))))

(define (%insert kx rkx s x t)
  (cond
        ((hamt-bitmap? t)
         (let* ((b (hamt-bitmap-bitmap t))
                (v (hamt-bitmap-vector t))
                (m (hamt/mask kx s))
                (i (hamt/mask-index b m)))
           (if (fix:= (fix:and b m) 0)
             (let* ((l (make-hamt-leaf kx rkx x))
                    (vp (vector-append (vector-head v i) (vector l) (vector-tail v i)))
                    (bp (fix:or b m)))
               (make-hamt-bitmap bp vp)
               )
             (let* ((st (vector-ref v i))
                    (stp (%insert kx rkx (+ s hamt/bits-per-subkey) x st))
                    (vp (vector-copy v)))
               (vector-set! vp i stp)
               (make-hamt-bitmap b vp)
               )
             )
           ))
        ((hamt-leaf? t)
         (let ((ky  (hamt-leaf-key t))
               (rky (hamt-leaf-real-key t))
               (y   (hamt-leaf-value t)))
           (if (fix:= ky kx)
             (if (eq? rky rkx)
               (make-hamt-leaf kx rkx x)
               (%insert kx rkx s x (make-hamt-collision kx (list (list rky y)))))
             (%insert kx rkx s x (make-hamt-bitmap (hamt/mask ky y) (vector t))))
           ))
        ((hamt-empty? t) (make-hamt-leaf kx rkx x))
        ((hamt-collision? t)
         (let ((alist (list-copy (hamt-collision-alist t))))
           (del-assq! rkx alist)
           (make-hamt-collision kx (cons (list rkx x) alist))))
        (else (error "invalid datatype passed to %insert"))
        )
  )

(define (%delete kx rkx s t)
  (cond ((hamt-empty? t) t)
        ((hamt-leaf? t)
         (let ((ky (hamt-leaf-key t))
               (rky (hamt-leaf-real-key t)))
           (if (and (fix:= kx ky) (eq? rkx rky))
             (make-hamt-empty)
             t)))
        ((hamt-bitmap? t)
         (let* ((b (hamt-bitmap-bitmap t))
                (v (hamt-bitmap-vector t))
                (m (hamt/mask kx s))
                (i (hamt/mask-index b m)))
           (if (fix:= (fix:and b m) 0)
             t
             (let* ((st (vector-ref v i))
                    (stp (%delete kx rkx (+ s hamt/bits-per-subkey) st)))
               (if (hamt-empty? stp)
                 (if (fix:= (popcount b) 1)
                   stp ;; (make-hamt-empty)
                   (make-hamt-bitmap
                     (fix:and (fix:not m) b)
                     (vector-append (vector-head v i) (vector-tail v (fix:+ i 1)))))
                 (if (eq? stp st)
                   t
                   (let ((vp (vector-copy v)))
                     (vector-set! vp i stp)
                     (make-hamt-bitmap b vp))))))))
        ((hamt-collision? t)
         (let ((alist (list-copy (hamt-collision-alist t))))
           (del-assq! rkx alist)
           (if (null? alist)
             (make-hamt-empty)
             (if (null? (cdr alist))
               (make-hamt-leaf kx (first (car alist)) (second (car alist)))
               (make-hamt-collision kx alist)))))
        (else (error "invalid tree passed to %delete"))
    ))

(define (hamt/lookup hamt key if-found if-not-found)
  (%lookup (hamt-hash key) key 0 hamt if-found if-not-found))

(define (hamt/get hamt key default)
  (hamt/lookup hamt key (lambda (x) x) (lambda () default)))

(define (hamt/insert hamt key datum)
  (%insert (hamt-hash key) key 0 datum hamt))

(define (hamt/delete hamt key)
  (%delete (hamt-hash key) key 0 hamt))

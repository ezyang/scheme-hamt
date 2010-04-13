(declare (usual-integrations))
(load-option 'wt-tree)

(define fixnum-max
    (let loop ((n 1))
       (if (fix:fixnum? n)
           (loop (* n 2))
           (- n 1))))

(define (mk-random-list k) (make-initialized-list k (lambda (x) (random fixnum-max))))

(define (printer run-time gc-time real-time)
  (begin
     ;(write (* 1000 (internal-time/ticks->seconds run-time)))
     ;(write-char #\,)
     ;(write (* 1000 (internal-time/ticks->seconds gc-time)))
     ;(write-char #\,)
     (write real-time)
     (newline)
     ))

(define (make-map g-insert g-empty vals)
    (fold-left (lambda (m x) (g-insert m x x)) (g-empty) vals))

(define (main-insert g-insert g-empty inserts)
  (let ((vals (mk-random-list inserts)))
    (with-timings (lambda () (let loop ((i 16000)) (make-map g-insert g-empty vals) (if (> i 0) (loop (- i 1))))) printer)
    )
  )

(define (make-lookup make-select-vals)
  (lambda (g-insert g-empty g-lookup inserts lookups)
      (let* ((vals (mk-random-list inserts))
             (select-vals (make-select-vals vals lookups))
             (m (make-map g-insert g-empty vals)))
        (with-timings
          (lambda ()
            (for-each
              (lambda (x)
                (g-lookup m x (lambda (x) x) (lambda () #f)))
              select-vals))
          printer))))

(define main-lookup-hit (make-lookup (lambda (vals lookups) (list-head (circular-list vals) lookups))))
(define main-lookup-miss (make-lookup (lambda (vals lookups) (mk-random-list lookups))))

#|
(define (main g-lookup g-insert g-empty i)
  (let* ((vals (mk-random-list i))
         (select-vals (append (list-tail vals (/ i 2)) (mk-random-list (/ i 2)))))
    (with-timings
      (lambda ()
        (let ((m (fold-left (lambda (m x) (g-insert m x x)) (g-empty) vals)))
          (fold-left (lambda (s k) (+ s (g-lookup m k (lambda (x) x) (lambda () 0)))) 0 select-vals)))
      printer)))
|#

(define (harness name g-lookup g-insert g-empty)
  (for-data-sets name (lambda (x) (main-insert g-insert g-empty x)))
  ;(for-data-sets name (lambda (x) (main-lookup-hit g-insert g-empty g-lookup x 128000)))
  ;(for-data-sets name (lambda (x) (main-lookup-miss g-insert g-empty g-lookup x 128000)))
  )

#|
(define (for-data-sets proc)
  (map
    (lambda (i)
      (let ((x (* 1000 (expt 2 i))))
        (write-line x)
        (proc x)))
    (iota 4 6)))
|#
(define (for-data-sets name proc)
  (map
    (lambda (i)
        (write-string name)
        (write-char #\,)
        (write i)
        (write-char #\,)
        (proc i))
    (iota 60 1)))

(harness "assoc"
         (lambda (m x sc fc) (let ((r (assq x m)))
                               (if (false? r)
                                 (fc)
                                 (sc (second r)))))
         (lambda (m k v) (cons (list k v) m))
         (lambda () '()))

(harness "wt-tree"
         (lambda (m x sc fc) (let ((r (wt-tree/lookup m x #f))) (if (false? r) (fc) (sc r))))
         wt-tree/add
         (lambda () (make-wt-tree (make-wt-tree-type
                                    (lambda (x y) (fix:< (eq-hash x)
                                                         (eq-hash y)))))))
(harness "hamt" hamt/lookup hamt/insert make-hamt)


(declare (usual-integrations))

(define (mk-random-list k) (make-initialized-list k (lambda (x) (random fixnum-max))))

(define (printer run-time gc-time real-time)
  (begin
     (write (internal-time/ticks->seconds run-time))
     (write-char #\space)
     (write (internal-time/ticks->seconds gc-time))
     (write-char #\space)
     (write (internal-time/ticks->seconds real-time))
     (newline)
     (newline)
     ))

(define (main g-lookup g-insert g-empty i)
  (let* ((vals (mk-random-list i))
         (select-vals (append (list-tail vals (/ i 2)) (mk-random-list (/ i 2)))))
    (with-timings
      (lambda ()
        (let ((m (fold-left (lambda (m x) (g-insert x x m)) (g-empty) vals)))
          (fold-left (lambda (s k) (+ s (g-lookup k m (lambda (x) x) (lambda () 0)))) 0 select-vals)))
      printer)))

(define (harness g-lookup g-insert g-empty)
    (map (lambda (i) (let ((x (* 1000 (expt 2 i)))) (write-line x) (main g-lookup g-insert g-empty x))) (iota 5 4)))

#| ;; association test (really slow!)
(harness (lambda (x m sc fc) (let ((r (assq x m)))
                               (if (false? r)
                                 (fc)
                                 (sc (second r)))))
         (lambda (k v m) (cons (list k v) m))
         (lambda () '()))
|#
(harness hamt-lookup hamt-insert make-hamt-empty)

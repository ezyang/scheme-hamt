(declare (usual-integrations))

(load "hamt")

(define fixnum-max
    (let loop ((n 1))
       (if (fix:fixnum? n)
           (loop (* n 2))
           (- n 1))))

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

(define (main i)
  (let* ((vals (mk-random-list i))
         (select-vals (append (list-tail vals (/ i 2)) (mk-random-list (/ i 2)))))
    (with-timings
      (lambda ()
        (let ((m (fold-left (lambda (m x) (hamt-insert x x m)) (make-hamt-empty) vals)))
          (fold-left (lambda (s k) (+ s (hamt-lookup k m (lambda (x) x) (lambda () 0)))) 0 select-vals)))
      printer)))

(map (lambda (i) (let ((x (* 1000 (expt 2 i)))) (write-line x) (main x))) (iota 5 5))

;; assumes 32-size bitstrings

;; Naive population count by manually testing each bit
(define (popcount-naive b)
  (define (accum i s)
    (if (= i 32)
      s
      (accum (+ i 1) (if (bit-string-ref b i) (+ s 1) s))))
  (accum 0 0))

;; Uses bit-substring-find-next-set-bit
(define (popcount-find-next b)
  (define (accum i s)
    (let ((r (bit-substring-find-next-set-bit b i 32)))
      (if (not (false? r)) (accum (+ r 1) (+ s 1)) s)))
  (accum 0 0))

(define printer (lambda (run-time gc-time real-time)
             (newline)
             (write (internal-time/ticks->seconds run-time))
             (write-char #\space)
             (write (internal-time/ticks->seconds gc-time))
             (write-char #\space)
             (write (internal-time/ticks->seconds real-time))
             (newline)))

(define (loop runnable)
  (lambda () (for-each runnable (iota 10000 900000000)))
  )

(define (quicktest reference other)
  (for-each (lambda (i)
              (let ((b (unsigned-integer->bit-string 32 i)))
                (if (= (reference b) (other b))
                  #t
                  (error "falsifiable with " i))))
            (iota 10000 900000000)))

;(quicktest popcount-naive popcount-naive)
;(quicktest popcount-naive popcount-find-next)

;(with-timings (loop (lambda (i) (popcount-naive (unsigned-integer->bit-string 32 i)))) printer)
;(with-timings (loop (lambda (i) (popcount-find-next (unsigned-integer->bit-string 32 i)))) printer)

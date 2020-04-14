(define (accumulate combiner start n term)
        (if (= n 0) start
            (combiner (term n) (accumulate combiner start (- n 1) term)))

    )

(define (accumulate-tail combiner start n term)
  (define (accumulate-tail-helper a x)
      (if (= a 0) x
          (accumulate-tail-helper (- a 1) (combiner (term a) x))))
  (accumulate-tail-helper n start)
)

(define (partial-sums stream)
  (define (helper n stream)
     (if (null? stream) nil
         (cons-stream (+ n (car stream))
                      (helper (+ n (car stream)) (cdr-stream stream)))))
  (helper 0 stream)
)

(define (rle s)
  (cond
    ((null? s) nil)
    (else (define (helper start num a)
            (cond
              ((null? a) (cons-stream (list start num) nil))
              ((= start (car a)) (helper (car a) (+ num 1) (cdr-stream a)))
              (else (cons-stream (list start num) (helper (car a) 1 (cdr-stream a))))))
          (helper (car s) 1 (cdr-stream s)))
  )
)

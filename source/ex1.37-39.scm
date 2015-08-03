(define (cont-frac n d k)
  (if (= k 1)
      (/ (n k) (d k))
    (/ (n k) (+ (d k) (cont-frac n d  (- k 1))))
    )
  )
(define (cont-frac-iter n d k)
  (define (iter k result)
    (if (= 1 k)
        (/ (n k) (+ (d k) result))
      (iter (- k 1) (/ (n k) (+ result (d k))))
      )
    )
  (iter k 0)
  )

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           25)

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                125
                )

(+ 2 (cont-frac-iter (lambda (i) 1.0)
                     (lambda (i) (if (= 2 (remainder i 3))
                                        (* 2 (/ (+ i 1) 3))
                                        1
                                        ))
                     25
                     ))

(define (tan-cf x k)
  (cont-frac-iter (lambda (i) (if (= i 1) 
                                x
                                (* x x -1)
                                ))
                  (lambda (i) (- (* 2 i) 1))
                  k
                  )
  )

(tan-cf 0.785398 20)

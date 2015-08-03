(define (f-r n)
  (if (< n 3)
      n
    (+ (f-r (- n 1))
       (* (f-r (- n 2)) 2)
       (* (f-r (- n 3)) 3)
       )
    )
  )
(define (f-iter n a b c)
  (if (< n 3)
      a
    (f-iter (- n 1)
             (+ a (* 2 b) (* 3 c))
             a
             b)
    )
  )
(define (f n) (f-iter n 2 1 0))

(f-r 4)
(f-iter 4 2 1 0)

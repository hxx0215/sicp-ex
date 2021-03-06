(define (cons p q)
  (* (expt 2 p)
     (expt 3 q)
     )
  )
(define (maxlog m n res)
  (if (= 0 (remainder m n))
    (maxlog (/ m n) n (+ res 1))
    res
    )
  )
(define (car m)
  (maxlog m 2 0)
  )
(define (cdr m)
  (maxlog m 3 0)
  )

(cdr 27)

(define (product term a next b)
  (if (> a b)
      1
    (* (term a)
       (product term (next a) next b)
       )
    )
  )
(define (inc n) (+ n 1))
(define (identy a) a)
(define (fact n)
  (product identy 1 inc n)
  )
(define (tm n)
  (if (even? n)
      (/ (+ n 2) (+ n 1))
    (/ (+ n 1) (+ n 2))
    )
  )
(define (q-pi m)
  (product tm 1 inc m)
  )

(fact 11)
(exact->inexact (* 4 (q-pi 30000)))


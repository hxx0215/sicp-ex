(define (same-parity header . lst)
  (define (same h l)
    (if (null? l) 
      (list)
      (if (even? (+ h (car l)))
        (cons (car l) (same h (cdr l)))
        (same h (cdr l))
        )
      )
    )
  (same header (cons header lst))
  )

(same-parity 2 3 4 5 6 7)


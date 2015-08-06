(define (car-n seq)
  (map car seq)
  )

(define (cdr-n seq)
  (map cdr seq)
  )

(cdr-n (list (list 1 2 3)
              (list 4 5 6)
              (list 7 8 9)
              ))

(define (mycarn seq)
  (if (null? seq)
    '()
    (cons (caar seq) (mycarn (cdr seq)))
    )
  )


(define (mycdrn seq)
  (if (null? seq)
    '()
    (cons (cdar seq) (mycdrn (cdr seq)))
    )
  )

(mycdrn (list (list 1 2 3)
              (list 4 5 6)
              (list 7 8 9)
              ))

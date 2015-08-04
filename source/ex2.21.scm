(define (square-list items)
  (if (null? items)
    nil
    (cons (square (car items)) (square-list (cdr items)))
    )
  )
(define (square a) (* a a))

(square-list (list 1 2 3))

(define (mapsquare items)
  (map square items)
  )

(mapsquare (list 1 2 3))

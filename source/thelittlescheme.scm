(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))
    )
  )

(cons 'a (car (list 'b 'c 'd)))

(eq? (cdr (list 'soured 'milk)) 'milk)

(define (lat? l)
  (if (null? l)
    true
    (and (atom? (car l)) (lat? (cdr l)))
    )
  )

(lat? (list 'Jack 'Sprat 'could 'eat 'no 'chicken 'fat))

(lat? (list '(Jack) 'Sprat 'could 'eat 'no 'chicken 'fat))

(lat? (list 'Jack '('Sprat 'could) 'eat 'no 'chicken 'fat))

(lat? '())

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

(define (rember a lat)
  (if (null? lat)
    '()
    (if (eq? a (car lat))
      (cdr lat)
      (cons (car lat) (rember a (cdr lat)))
      )
    )
  )

(rember 'mint '(lamb chops and mint jelly))

(rember 'mint '(lamb chops and mint flavored mint jelly))

(rember 'toast '(bacon lettuce and tomato))

(define (firsts l)
  (if (null? l)
    '()
    (cons () (firsts (cdr l)))
    )
  )

(firsts '((apple peach pumpkin)
        (plum pear cherry)
        (grape raisin pea)
        (bean carrot eggplant)
        ))

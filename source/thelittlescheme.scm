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
  (map car l)
  )

(firsts (list (list 'apple 'peach 'pumpkin)
        (list 'plum 'pear 'cherry)
        (list 'grape 'raisin 'pea)
        (list 'bean 'carrot 'eggplant)
        ))

(define (insertR new old lat)
  (if (null? lat)
    '()
    (cons (car lat)
          (if (eq? (car lat) old)
            (cons new (cdr lat))
            (insertR new old (cdr lat))
            )
          )
    )
  )

(define (insertL new old lat)
  (if (null? lat)
    '()
    (if (eq? (car lat) old)
      (cons new lat)
      (cons (car lat) (insertL new old (cdr lat)))
      )
    )
  )

(define (subst new old lat)
  (if (null? lat)
    '()
    (if (eq? (car lat) old)
      (cons new (cdr lat))
      (cons (car lat) (subst new old (cdr lat)))
      )
    )
  )

(define (subst2 new o1 o2 lat)
  (if (null? lat)
    '()
    (if (or (eq? o1 (car lat) (eq? o2 (car lat))))
      (cons new (cdr lat))
      (cons (car (subst2 new o1 o2 (cdr lat))))
      )
    )
  )
(insertR 'topping 'fudge '(ice cream with fudge for dessert))

(insertR 'jalapeno 'and '(tacos tamales and salsa))

(insertR 'e 'd '(a b c d f g d h))

(define multirember
  (lambda (a lat)
    (if (null? lat)
      '()
      (if (eq? a (car lat))
        (multirember a (cdr lat))
        (cons (car lat) (multirember a (cdr lat)))
        )
      )
    )
  )

(multirember 'cup '(coffee cup tea cup and hick cup))

(define multiinsertR
  (lambda (new old lat)
    (if (null? lat)
      '()
      (if (eq? old (car lat))
        (cons (car lat) (cons new (multiinsertR old new (cdr lat))))
        (cons (car lat) (multiinsertR new old (cdr lat)))
        )
      )
    )
  )

(define multiinsertL
  (lambda (new old lat)
    (if (null? lat)
      '()
      (if (eq? old (car lat))
        (cons new (cons old (multiinsertL new old (cdr lat))))
        (cons (car lat) (multiinsertL new old (cdr lat)))
        )
      )
    )
  )

(define multisubst
  (lambda (new old lat)
    (if (null? lat)
      '()
      (if (eq? old (car lat))
        (cons new (multisubst new old (cdr lat)))
        (cons (car lat) (multisubst new old (cdr lat)))
        )
      )
    )
  )

(define add1
  (lambda (n)
    (+ n 1)
    )
  )

(define sub1
  (lambda (n)
    (- n 1)
    )
  )

(define plus
  (lambda (a b)
    (if (zero? b)
      a
      (add1 (plus a (sub1 b)))
      )
    )
  )

(plus 9 4)

(define minus
  (lambda (m n)
    (if (zero? n)
      m
      (sub1 (minus m (sub1 n)))
      )
    )
  )

(minus 1000 9)

(define addtup
  (lambda (l)
    (if (null? l)
      0
      (+ (car l) (addtup (cdr l)))
      )
    )
  )

(addtup (list 10 2 3 4 6))

(define multi
  (lambda (a b)
    (if (zero? b)
      0
      (+ a (multi a (sub1 b)))
      )
    )
  )


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

(define tup+
  (lambda (tup1 tup2)
    (if (null? tup1)
        tup2
      (if (null? tup2)
          tup1
        (cons (+ (car tup1) (car tup2))
              (tup+ (cdr tup1) (cdr tup2))
              )
        )
      )
    )
  )

(tup+ (list 3 6 9 11 4)
      (list 8 5 2 0 7)
      )

(tup+ (list 2 3)
      (list 4 6)
      )

(tup+ (list 3 7)
      (list 4 6 8 1)
      )

(tup+ (list 3 7 8 1)
      (list 4 6)
      )

(define small
  (lambda (a b)
    (if (zero? b)
      #f
      (if (zero? a)
        #t
        (small (sub1 a) (sub1 b))
        )
      )
    )
  )

(define ^
  (lambda (a b)
    (if (zero? b)
      1
      (* a (^ a (sub1 b)))
      )
    )
  )

(define div
  (lambda (a b)
    (if (small a b)
      0
      (add1 (div (- a b) b))
      )
    )
  )

(div 16 4)

(define length-lat 
  (lambda (lat)
    (if (null? lat)
      0
      (add1 (length-lat (cdr lat)))
      )
    )
  )

(length-lat '(hotdogs with mustard sauerkraut and pickles))

(length-lat '(ham and chees on rye))

(define pick
  (lambda (n lat)
    (if (null? lat)
      '()
      (if (zero? (sub1 n))
        (car lat)
        (pick (sub1 n) (cdr lat))
        )
      )
    )
  )

(pick 4 '(lasagna spaghetti ravioli macaroni meatball))

(define rempick
  (lambda (n lat)
    (if (null? lat)
      '()
      (if (zero? (sub1 n))
        (cdr lat)
        (cons (car lat) (rempick (sub1 n) (cdr lat)))
        )
      )
    )
  )

(rempick 3 '(hotdogs with hot mustard))

(define no-nums
  (lambda (lat)
    (if (null? lat)
      '()
      (if (number? (car lat))
        (no-nums (cdr lat))
        (cons (car lat) (no-nums (cdr lat)))
        )
      )
    )
  )

(no-nums '(5 pears 6 prunes 9 dates))

(define eqan?
  (lambda (a b)
    (if (and (number? a) (number? b))
      (= a b)
      (if (or (number? a) (number? b))
        false
        (eq? a b)
        )
      )
    )
  )

(define occur
  (lambda (a lat)
    (if (null? lat)
      0
      (if (eq? a (car lat))
        (add1 (occur a (cdr lat)))
        (occur a (cdr lat))
        )
      )
    )
  )

(occur 'test '(test test test test))

(define rember*
  (lambda (a l)
    (if (null? l)
      '()
      (if (atom? (car l))
        (if (eq? a (car l))
          (rember* a (cdr l))
          (cons (car l) (rember* a (cdr l)))
          )
       (cons (rember* a (car l)) (rember* a (cdr l)))
        )
      )
    )
  )

(rember* 'sauce '(((tomato sauce))
                  ((bean) sauce)
                  (and ((flying)) sauce)
                  ))

(define insertR*
  (lambda (new old l)
    (if (null? l)
        '()
      (if (atom? (car l))
          (if (eq? old (car l))
              (cons old (cons new (insertR* new old (cdr l))))
            (cons (car l) (insertR* new old (cdr l)))
            )
        (cons (insertR* new old (car l)) (insertR* new old (cdr l)))
        )
      )
    )
  )

(insertR* 'roast 'chuck '((how much (wood))
                        could
                        ((a (wood) chuck))
                        (((chuck)))
                        (if (a) ((wood chuck)))
                        could chuck wood
                        ))

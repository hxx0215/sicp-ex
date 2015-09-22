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

(define occur*
  (lambda (a l)
    (if (null? l)
      0
      (if (atom? (car l))
        (if (eq? a (car l))
          (add1 (occur* a (cdr l)))
          (occur* a (cdr l))
          )
        (+ (occur* a (car l)) (occur* a (cdr l)))
        )
      )
    )
  )

(occur* 'banana '((banana)
                  (split ((((banana ice)))
                          (cream (banana))
                          sherbet
                          ))
                  (banana)
                  (bread)
                  (banana brandy)
                  ))

(define subst*
  (lambda (new old l)
    (if (null? l)
      '()
      (if (atom? (car l))
        (if (eq? old (car l))
          (cons new (subst* new old (cdr l)))
          (cons (car l) (subst* new old (cdr l)))
          )
        (cons (subst* new old (car l))
              (subst* new old (cdr l))
              )
        )
      )
    )
  )

(subst* 'orange 'banana '((banana)
                          (split ((((banana ice)))
                                  (cream (banana))
                                  sherbet
                                  ))
                          (banana)
                          (bread)
                          (banana brandy)
                          ))

(define insertL*
  (lambda (new old l)
    (if (null? l)
      '()
      (if (atom? (car l))
        (if (eq? old (car l))
          (cons new (cons old (insertL* new old (cdr l))))
          (cons (car l) (insertL* new old (cdr l)))
          )
        (cons (insertL* new old (car l))
              (insertL* new old (cdr l))
              )
        )
      )
    )
  )

(insertL* 'pecker 'chuck '((how much (wood))
                         could
                         ((a (wood) chuck))
                         (if (a) ((wood chuck)))
                         could chuck wood
                         ))

(define member* 
  (lambda (a l)
    (if (null? l)
      false
      (if (atom? (car l))
        (if (eq? a (car l))
          true
          (member* a (cdr l))
          )
        (or (member* a (car l)) (member* a (cdr l)))
        )
      )
    )
  )

(member* 'chips '((potato) (chips ((with) fish) (chips))))

(define eqlist?
  (lambda (l1 l2)
    (if (and (null? l1) (null? l2))
      true
      (if (or (null? l1) (null? l2))
        false
        (if (and (atom? (car l1)) (atom? (car l2)))
          (and (eqan? (car l1) (car l2))
               (eqlist? (cdr l1) (cdr l2))
          )
          (if (or (atom? (car l1)) (atom? (car l2)))
            false
            (and (eqlist? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2))
                 )
            )
        )
      )
    )
  )
)

(eqlist? '(strawberry ice cream) 
         '(strawberry ice cream))

(eqlist? '(banana ((split)))
         '((banana) (split)))

(eqlist? '(beef ((sausage)) (and (soda)))
         '(beef ((sausage)) (and (soda))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
        (and (numbered? (car aexp))
             (numbered? (car (cdr (cdr aexp))))
             )
        )
      )
    )
  )


(define set?
  (lambda (lat)
    (if (null? lat)
      #t
      (and (not (member* (car lat) (cdr lat)))
           (set? (cdr lat))
           )
      )
    )
  )

(set? '(apple peaches pears plums))

(define makeset
  (lambda (lat)
    (if (null? lat)
      '()
      (if (member* (car lat) (cdr lat))
        (makeset (cdr lat))
        (cons (car lat) (makeset (cdr lat)))
        )
      )
    )
  )

(makeset '(apple peach pear peach plum apple lemon peach))

(define makeset2
  (lambda (lat)
    (if (null? lat)
      '()
      (cons (car lat) (makeset2 (multirember (car lat) (cdr lat))))
      )
    )
  )

(makeset2 '(apple peach pear peach plum apple lemon peach))

(define subset?
  (lambda (set1 set2)
    (if (null? set1)
      #t
      (and (member* (car set1) set2)
           (subset? (cdr set1) set2)
           )
      )
    )
  )

(subset? '(5 chicken wings)
         '(5 hamburgers
           2 pieces fried chicken and
           light duckling wings
           ))

(subset? '(4 punds of horseradish)
         '(four punds chicken and
                5 ounces horseradish
                ))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1)
         )
    )
  )

(eqset? '(6 large chickens with wings)
        '(6 chickens with large wings))

(define intersect?
  (lambda (set1 set2)
    (if (null? set1)
      #f
      (or (member* (car set1) set2)
          (intersect? (cdr set1) set2)
          )
      )
    )
  )

(intersect? '(stewed tomatoes)
            '(macaroni and cheese))

(define intersect
  (lambda (set1 set2)
    (if (null? set1)
      '()
      (if (member* (car set1) set2)
        (cons (car set1) (intersect (cdr set1) set2))
        (intersect (cdr set1) set2)
        )
      )
    )
  )

(intersect '(stewed tomatoes and macaroni)
           '(macaroni and cheese)
           )

(define union
  (lambda (set1 set2)
    (if (null? set1)
      set2
      (if (member* (car set1) set2)
        (union (cdr set1) set2)
        (cons (car set1) (union (cdr set1) set2))
        )
      )
    )
  )

(union '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))

(define intersectall 
  (lambda (l-set)
    (if (null? (cdr l-set))
      (car l-set)
      (intersect (car l-set)
                 (intersectall (cdr l-set))
                 )
      )
    )
  )

(intersectall '((6 pears and)
                (3 peaches and 6 peppers)
                (8 pears and 6 plums)
                (and 6 prunes with some apples)
                ))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f)
      )
    )
  )

(a-pair? '(3 7))

(a-pair? '((2) (pair)))

(a-pair? '(full (house)))

(define first
  (lambda (p)
    (car p)
    )
  )
(define second
  (lambda (p)
    (car (cdr p))
    )
  )
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))
    )
  )
(define third
  (lambda (x)
    (car (cdr (cdr x)))
    )
  )

(define fun?
  (lambda (rel)
    (set? (firsts rel))
    )
  )

(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))

(define revrel
  (lambda (rel)
    (if (null? rel)
      '()
      (cons (build (second (car rel))  
                   (first (car rel))
                  )
            (revrel (cdr rel))
            )
      )
    )
  )

(revrel '((8 a) (pumpkin pie) (got sick)))

(define revpair
  (lambda (pair)
    (build (second pair)
           (first pair)
           )
    )
  )

(define seconds
  (lambda (l)
    (map cadr l)
    )
  )

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))
    )
  )

(define fun?
  (lambda (rel)
    (set? (firsts fun))
    )
  )

(fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))

(define rember-f
  (lambda (test? a l)
    (if (null? l)
      '()
      (if (test? a (car l))
        (cdr l)
        (rember-f test? a (cdr l))
        )
      )
    )
  )

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a)
      )
    )
  )

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (if (null? l)
        '()
        (if (test? a (car l))
          (cdr l)
          (cons (car l) 
                ((rember-f test?) a (cdr l)))
          )
        )
      )
    )
  )

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (if (null? l)
        '()
        (if (test? old (car l))
          (cons new (cons old (cdr l)))
          (cons (car l) ((insertL-f test?) new old (cdr l)))
          )
        )
      )
    )
  )


(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (if (null? l)
        '()
        (if (test? old (car l))
          (cons old (cons new (cdr l)))
          (cons (car l) ((insertR-f test?) new old (cdr l)))
          )
        )
      )
    )
  )

(define seqL
  (lambda (new old l)
    (cons new (cons old l))
    )
  )

(define seqR
  (lambda (new old l)
    (cons old (cons new l))
    )
  )

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (if (null? l)
        '()
        (if (eq? (car l) old)
          (seq new old (cdr l))
          (cons (car l) ((insert-g seq) new old (cdr l)))
          )
        )
      )
    )
  )

(define evens-only*
  (lambda (lat)
    (if (null? lat)
      '()
      (if (atom? (car lat))
        (if (even? (car lat))
          (cons (car lat) (evens-only* (cdr lat)))
          (evens-only* (cdr lat))
          )
        (cons (evens-only* (car lat))
              (evens-only* (cdr lat))
              )
        )
      )
    )
  )

(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

(define evens-only*co
  (lambda (l col)
    (if (null? l)
      (col '() 1 0)
      (if (atom? (car l))
        (if (even? (car l))
          (evens-only*co (cdr l)
                         (lambda (newl p s)
                          (col (cons (car l)
                                     newl
                                     )
                               (* (car l) p)
                               s
                               )
                           )
                         )
          (evens-only*co (cdr l)
                         (lambda (newl p s)
                           (col newl p (+ (car l) s))
                           )
                         )
          )
        (evens-only*co (car l)
                       (lambda (al ap as)
                         (evens-only*co (cdr l)
                                        (lambda (dl dp ds)
                                          (col (cons al dl)
                                               (* ap dp)
                                               (+ as ds)
                                               )
                                          )
                                        )
                         )
                       )
        )
      )
    )
  )

(evens-only*co '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
               (lambda (newl product sum)
                 (cons sum (cons product newl))
                 )
               )

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)
    )
  )
(define keep-looking
  (lambda (a i lat)
    (if (eq? a i)
      #t
      (if (number? i)
        (keep-looking a (pick i lat) lat)
        #f
        )
      ) 
    )
  )

(looking 'caviar
         '(6 2 grits caviar 5 7 3)
         )

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)
                  )
           )
    )
  )

(define align
  (lambda (pora)
    (if (atom? pora) 
      pora
      (if (a-pair? (first pora))
        (align (shift pora))
        (build (first pora)
               (align (second pora))
               )
        )
      )
    )
  )

(align '((a (b c)) d))

(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1
            ((lambda (l)
               (cond 
                 ((null? l) 0)
                 (else (add1 
                         (eternity (cdr l))
                         ))
                 )
               ) (cdr l))
            ))
    )
  )

(define eternity
  (lambda (x)
    (eternity x)
    )
  )
(lambda (l)
  (cond 
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))
    )
  )
(
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))
      )
     )
   )
 eternity
 )
(
 (lambda (mk-length)
   (mk-length mk-length)
   )
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (mk-length (cdr l))))
       )
     )
   )
 )


(((lambda (mk-length)
    (mk-length mk-length)
    )
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1
                (;;(mk-length mk-length)
                 ;;(mk-length mk-length)
                 (lambda (x)
                   ((mk-length mk-length) x)
                   )
                 (cdr l)
                 )
                ))
        )
      )
    )
  )
 '(apple orange)
 )

(
 (
  (lambda (mk-length)
    (mk-length mk-length)
    )
  (lambda (mk-length)
    (
     (lambda (length)
       (lambda (l)
         (cond
          ((null? l) 0)
          (else
           (add1 (length (cdr l)))
           )
          )
         )
       )
     (lambda (x)
       ((mk-length mk-length) x)
       )
     )
    )
  ) '(apple pear))

((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length)
      )
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x)
            ))
      )
    )
   )
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))
       )
     )
   )
 )

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))
       )
     )
    )
  )

((y (lambda (len)
     (lambda (l)
       (if (null? l)
         0
         (add1 (len (cdr l)))
         )
       )
     ))
 '(a b c))

(Y (Y Y))

(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f
                          )
    )
  )
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? name (car names)) (car values))
      (else
        (lookup-in-entry-help 
          name
          (cdr names)
          (cdr values)
          entry-f
          )
        )
      )
    )
  )

(define extend-table cons)

(define lookup-in-table 
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else
        (lookup-in-entry name
                         (car table)
                         (lambda (name)
                           (lookup-in-table
                                      name
                                     (cdr table) 
                                     table-f
                                      )
                           )
                         )
        )
      )
    )
  )

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e))
      )
    )
  )
(define atom-to-action 
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      (else *identifier)
      )
    )
  )
(define list-to-action 
  (lambda (e)
    (cond
      ((atom? (car e)) 
       (cond
         ((eq? (car e) 'quote) *quote)
         ((eq? (car e) 'lambda) *lambda)
         ((eq? (car e) 'cond) *cond)
         (else *application)
         )
       )
      (else *application)
      )
    )
  )
(define value
  (lambda (e)
    (meaning e (quote ()))
    )
  )
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)
    )
  )
(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e))
      )
    )
  )
(define *quote
  (lambda (e table)
    (text-of e)
    )
  )
(define text-of second)
(define *identifier 
  (lambda (e table)
    (lookup-in-table e table initial-table)
    )
  )
(define initial-table
  (lambda (name)
    (car (quote ()))
    )
  )
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e))
           )
    )
  )
(define table-of first)
(define formals-of second)
(define body-of third)
(define evcon
  (lambda (lines table)
    (cond 
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
                table
                )
       )
      ((meaning (question-of (car lines))
                table
                )
       (meaning (answer-of (car lines))
                table
                )
       )
      (else (evcon (cdr lines) table) )
      )
    )
  )

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)
    )
  )

(define cond-lines-of cdr)
(define evlis
  (lambda (args table)
    (cond 
      ((null? args) (quote()))
      (else
        (cons (meaning (car args) table)
              (evlis (cdr args) table)
              )
        )
      )
    )
  )
(define *application
  (lambda (e table)
    (apply
      (meaning (function-of e) table)
      (evlis (arguments-of e) table)
      )
    )
  )
(define function-of car)
(define arguments-of cdr)
(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))
    )
  )


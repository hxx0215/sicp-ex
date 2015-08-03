(define (last-pair l)
  (if (= 1 (length l))
    (car l)
    (last-pair (cdr l))
    )
  )

(last-pair (list 1 23 3 5))

(define (reverse-list l)
  (define (iter lst result)
    (if (null? lst)
      result
      (iter (cdr lst)
            (cons (car lst) result)
            )
      )
    )
  (iter l '())
  )

(reverse-list )

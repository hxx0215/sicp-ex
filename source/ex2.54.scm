(define (equal?-ex a b)
  (if (and (symbol? a) (symbol? b))
      (eq? a b)
    (if (and (list? a) (list? b))
        (if (and (null? a) (null? b))
            #t
          (if (or (null? a) (null? b))
              #f
            (if (equal?-ex (car a) (car b))
                (equal?-ex (cdr a) (cdr b))
              #f
              )
            )
          )
      )
    )
  )

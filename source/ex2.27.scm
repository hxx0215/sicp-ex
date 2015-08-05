(define (deep-reverse l)
  (if (null? l)
    '()
    (if (pair? l)
      (reverse (list (deep-reverse (car l)) (deep-reverse (cadr l))))
      l
      )
    )
  )

(reverse (list (list 1 2) (list 3 4)))

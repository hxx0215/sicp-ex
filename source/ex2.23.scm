(define (foreach f l)
  (if (null? l)
    (newline)
    (begin
     (f (car l))
     (foreach f (cdr l))
     )
    )
  )

(for-each (lambda (x) (newline) (display x))
         (list 1 2 3)
         )

(define (enumerate-interval low high)
  (if (> low high)
     '() 
    (cons low (enumerate-interval (+ low 1) high))
    )
  )
(define (fold-right op initial lst)
  (if (null? lst)
    initial
    (op (car lst)
        (fold-right op initial (cdr lst))
        )
    )
  )
(define (flatmap proc seq)
  (fold-right append '() (map proc seq))
  )
(define (three n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                    (map (lambda (k)
                           (list i j k)
                           )
                         (enumerate-interval 1 (- j 1))
                         )
                    )
                  (enumerate-interval 1 (- i 1))
                  )
             )
           (enumerate-interval 1 n)
           )
  )
(define (sum pair)
  (fold-right + 0 pair)
  )
(define (sumequal s pair)
  (= s (sum pair))
  )
(define (triple n s)
  (filter (lambda (pair)
            (sumequal s pair)
            ) (three n))
  )

(triple 13 10)

;unique-triple answer 
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j)
                    )
                  (enumerate-interval 1 (- i 1))
                  )
             )
           (enumerate-interval 1 n)
           )
  )
(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (cons i j)
                    )
                  (unique-pairs (- i 1))
                  )
             )
           (enumerate-interval 1 n)
           )
  )

(unique-triples 6)
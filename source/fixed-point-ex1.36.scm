(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
    )
  (define (try guess step)
    (display step)
    (display ":")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
        (try next (+ step 1))
        )
      )
    )
  (try first-guess 1)
  )

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0
             )

(define (average a b) 
    (/ (+ a b) 2)
    )

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0
               )
  )
(sqrt 2.0)
(fixed-point (lambda (x) (+ 1 (/ 1 x)))
             1.0
             )

(fixed-point (lambda (x) (/ (log 1000) (log x)))
             10.0
             )

(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
             10.0
             )

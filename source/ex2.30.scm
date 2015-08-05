(define (squrare-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (squrare-tree sub-tree)
           (* sub-tree sub-tree)
           )
         ) tree)
  )

(squrare-tree 
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7))
  )

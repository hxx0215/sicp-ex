(define (make-leaf symbol weight)
  (list 'leaf symbol weight)
  )
(define (leaf? object)
  (eq? (car object) 'leaf)
  )
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))
        )
  )
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
    (caddr tree)
    )
  )
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
    (cadddr tree)
    )
  )
(define (decode bits tree)
  (define (decode-l bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-l (cdr bits) tree)
                )
          (decode-l (cdr bits) next-branch)
          )
        )
      )
    )
  (decode-l bits tree)
  )
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))
        )
  )

(define smaple-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree 
                    (make-leaf 'B 2)
                    (make-code-tree 
                      (make-leaf 'D 1)
                      (make-leaf 'C 1))
                    )
                  )
  )
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message smaple-tree)


(define (find-symbol symbol tree path)
  (if (leaf? tree)
    (if (eq? (symbol-leaf tree) symbol)
      path
      '()
      )
    (append (find-symbol symbol (left-branch tree) (append path (list 0))) 
            (find-symbol symbol (right-branch tree) (append path (list 1)))
            )
    )
  )
(define (encode-symbol symbol tree)
  (if (null? (find-symbol symbol tree '()))
    (error "bad bit -- CHOOSE-BRANCH" bit)
    (find-symbol symbol tree '())
    )
  )
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree)
            )
    )
  )

(encode (decode sample-message smaple-tree) smaple-tree)

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))
                    ))
        )
  )
(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)
                             (cadr pair)
                             )
                  (make-leaf-set (cdr pairs))
                  )
      )
    )
  )

(define sample-pairs (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs))
  )

(define (successive-merge l)
  (if (= 1 (length l))
    (car l)
    (successive-merge (adjoin-set 
                       (make-code-tree (car l) (cadr l)) 
                       (cddr l)
                        ))
    )
  )

(generate-huffman-tree sample-pairs)

(define rock-alphabet (list (list 'A 2) (list 'NA 16) (list 'BOOM 1) (list 'SHA 3) (list 'GET 2) (list 'YIP 9) (list 'JOB 2) (list 'WAH 1)))

(generate-huffman-tree rock-alphabet)
(define rock-message '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))

(encode rock-message (generate-huffman-tree rock-alphabet))

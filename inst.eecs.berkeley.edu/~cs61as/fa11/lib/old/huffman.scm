(define (make-leaf symbol weight)
   (list 'leaf symbol weight))

(define (leaf? object)
   (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
   (list left
         right
         (append (symbols left) (symbols right))
         (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
   (if (leaf? tree)
       (list (symbol-leaf tree))
       (caddr tree)))

(define (weight tree)
   (if (leaf? tree)
       (weight-leaf tree)
       (cadddr tree)))

(define (adjoin-set x set)
   (cond ((null? set) (list x))
         ((< (weight x) (weight (car set))) (cons x set))
         (else (cons (car set)
                     (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
   (if (null? pairs)
       '()
       (let ((pair (car pairs)))
         (adjoin-set (make-leaf (car pair)     ;symbol
                                (cadr pair))   ;frequency
                     (make-leaf-set (cdr pairs))))))

(define (decode bits tree)
  (decode-1 bits tree tree))

(define (decode-1 bits tree current-branch)
  (if (null? bits)
      '()
      (let ((next-branch
             (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
            (cons (symbol-leaf next-branch)
                  (decode-1 (cdr bits) tree tree))
            (decode-1 (cdr bits) tree next-branch)))))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

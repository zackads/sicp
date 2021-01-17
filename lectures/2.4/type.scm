(define (make-regular root)
  (attach-type 'regular root))

(define (make-es root)
  (attach-type 'es root))

(define (make-irreg fp sp tp)
  (attach-type 'irreg (list fp sp tp)))

(define (attach-type type contents)
  (cons type contents))

(define (type thing)
  (car thing))

(define (contents thing)
  (cdr thing))

(define (fps verb)
  (cond ((regular? verb) (root (contents verb)))
	((es? verb) (root (contents verb)))
	((irreg? verb) (fp (contents verb)))))

(define (sps verb)
  (cond ((regular? verb) (root (contents verb)))
	((es? verb) (root (contents verb)))
	((irreg? verb) (sp (contents verb)))))

(define (tps verb)
  (cond ((regular? verb) (add-s (root (contents verb))))
	((es? verb) (add-es (root (contents verb))))
	((irreg? verb) (tp (contents verb)))))

(define (fpp verb)
  (cond ((regular? verb) (root (contents verb)))
	((es? verb) (root (contents verb)))
	((irreg? verb) (sp (contents verb)))))

(define (spp verb)
  (cond ((regular? verb) (root (contents verb)))
	((es? verb) (root (contents verb)))
	((irreg? verb) (sp (contents verb)))))

(define (tpp verb)
  (cond ((regular? verb) (root (contents verb)))
	((es? verb) (root (contents verb)))
	((irreg? verb) (sp (contents verb)))))

(define (regular? verb)
  (eq? (type verb) 'regular))

(define (es? verb)
  (eq? (type verb) 'es))

(define (irreg? verb)
  (eq? (type verb) 'irreg))

(define (root verb)
  verb)

(define (fp verb)
  (car verb))

(define (sp verb)
  (cadr verb))

(define (tp verb)
  (caddr verb))

(define (add-s wd)
  (word wd 's))

(define (add-es wd)
  (word wd 'es))

(define eat (make-regular 'eat))
(define drink (make-regular 'drink))
(define go (make-es 'go))
(define box (make-es 'box))
(define be (make-irreg 'am 'are 'is))




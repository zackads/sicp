(define (lint n)
    (define (lint-help n)
        (define wd (word 'int- n '.scm))
        (define filename (symbol->string wd))
        (if (= n 1)
            (load filename)
            (begin
                (lint (- n 1))
                (load filename))))
    (begin
        (load "record.scm")
        (lint-help n)))


(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assq key-1 (cdr local-table))))
        (if (null? subtable)
            nil
            (let ((record (assq key-2 (cdr subtable))))
              (if (null? record)
                  nil
                  (cdr record))))))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assq key-1 (cdr local-table))))
        (if (null? subtable)
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))
            (let ((record (assq key-2 (cdr subtable))))
              (if (null? record)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))
                  (set-cdr! record value)))))
       'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))


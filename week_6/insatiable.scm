#lang racket/gui

(require berkeley)

; Data structures of each department's records
(define (make-record-marketing name address salary)
  (list name address salary))
(define (make-record-finance name address salary)
  (cons name (cons address salary)))
(define (make-personnel-file-marketing . records)
  (type-tag 'marketing-file records))
(define (make-personnel-file-finance . records)
  (type-tag 'finance-file (lambda (name)
                            (filter (lambda (record) (eq? (car record) name)) records))))

; Define map of types and operations
(put 'marketing-file 'get-record
     (lambda (name personnel-file)
       (filter (lambda (record) (eq? (content (car record)) name)) personnel-file)))
(put 'marketing-file 'get-salary
     (lambda (name personnel-file)
       (caddr (get-record name personnel-file))))
(put 'finance-file 'get-record
     (lambda (name personnel-file)
       (personnel-file name)))
(put 'finance-file 'get-salary
     (lambda (name personnel-file)
       (cadr (get-record name-personnel-file))))
(put 'new-company-file 'get-record
     ; procedure to access new company's ADT here
     )
(put 'new-company-file 'get-salary
     ; procedure to access new company's ADT here
     )


; Generic procedure for getting an employee record
; Works for both marketing and finance departments
(define (get-record employee-name personnel-file)
  (let ((get-record-proc (get (type-tag personnel-file) 'get-record)))
    (if get-record-proc
        (get-record-proc employee-name (contents personnel-file))
        (error "Unknown operator for type"))))
        
; Generic procedure for getting an employee's salary
(define (get-salary employee-name personnel-file)
  (let ((get-salary-proc (get (type-tag personnel-file) 'get-salary)))
    (if get-salary-proc
        (get-salary-proc employee-name (contents personnel-file))
        (error "Unknown operator for type"))))

; Generic procedure for finding an employee from a list of personnel files
(define (find-employee-record employee-name personnel-files)
  (let ((record (get-record employee-name (car personnel-files)))
        (if record
            record
            (find-employee-record (cdr personnel-files))))))
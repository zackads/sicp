# CS61A - Week 6 Homework

## SICP exercises

**SICP Exercise 2.74** - Insatiable Enterprises, Inc., is a highly de-centralized conglomerate company consisting of a large number of independent divisions located all over the world. The company’s computer facilities have just been interconnected by means of a clever network-interfacing scheme that makes the entire network appear to any user to be a single computer. Insatiable’s president, in her first attempt to exploit the ability of the network to extract administrative information from division files, is dismayed to discover that, although all the division files have been implemented as data structures in Scheme, the particular data structure used varies from division to division. A meeting of division managers is hastily called to search for a strategy to integrate the files that will satisfy headquarters’ needs while preserving the existing autonomy of the divisions.

Show how such a strategy can be implemented with data-directed programming. As an example, suppose that each division’s personnel records consist of a single file, which contains a set of records keyed on employees’ names. The structure of the set varies from division to division. Furthermore, each employee’s record is itself a set (structured differently from division to division) that contains information keyed under identifiers such as address and salary. In particular:

a. Implement for headquarters a `get-record` procedure that retrieves a specified employee’s record from a specified personnel file. The procedure should be applicable to any division’s file. Explain how the individual divisions’ files should be structured. In particular, what type information must be supplied?

> See `insatiable.scm`
>
> ```scheme
> ; Data structures of each department's records
> (define (make-record-marketing name address salary)
>  (list name address salary))
> (define (make-record-finance name address salary)
>  (cons name (cons address salary)))
> (define (make-personnel-file-marketing . records)
>  (type-tag 'marketing-file records)
> (define (make-personnel-file-finance . records)
>  (type-tag 'finance-file (lambda (name)
>    (filter (lambda (record) (eq? (car record) name)) records))))
>
> ; Define map of types and operations
> (put 'marketing-file 'get-record
>     (lambda (name personnel-file)
>       (filter (lambda (record) (eq? (content (car record)) name)) personnel-file)))
> (put 'finance-file 'get-record
>     (lambda (name personnel-file)
>       (personnel-file name)))
>
> ; Generic procedure for getting an employee record
> ; Works for both marketing and finance departments
> (define (get-record employee-name personnel-file)
>  (let ((get-record-proc (get (type-tag personnel-file) 'get-record)))
>    (if get-record-proc
>        (get-record-proc employee-name (contents personnel-file))
>        (error "Unknown operator for type"))))
> ```
>
> Only the `'marketing-file` and `'finance-file` type tags must be supplied. `name`, `address` and `salary` fields exist below the abstraction barrier, so only the constructors and retrievers of the two personnel file types need know about the underlying data structure. `'marketing-file` and `'finance-file` types need to be public so `get-record` can `get` the correct retriever from the type table.

b. Implement for headquarters a `get-salary` procedure that returns the salary information from a given employee’s record from any division’s personnel file. How should the record be structured in order to make this operation work?

> See `insatiable.scm`
>
> ```scheme
> ; Define map of types and operations
> (put 'marketing-file 'get-salary
>     (lambda (name personnel-file)
>       (caddr (get-record name personnel-file))))
> (put 'finance-file 'get-salary
>     (lambda (name personnel-file)
>       (cadr (get-record name-personnel-file))))
>
> ; Generic procedure for getting an employee's salary
> (define (get-salary employee-name personnel-file)
>  (let ((get-salary-proc (get (type-tag personnel-file) 'get-salary)))
>    (if get-salary-proc
>        (get-salary-proc employee-name (contents personnel-file))
>        (error "Unknown operator for type"))))
> ```

c. Implement for headquarters a `find-employee-record` procedure. This should search all the divisions’ files for the record of a given employee and return the record. Assume that this procedure takes as arguments an employee’s name and a list of all the divisions’ files.

> See `insatiable.scm`
>
> ```scheme
> ; Generic procedure for finding an employee from a list of personnel files
> (define (find-employee-record employee-name personnel-files)
>  (let ((record (get-record employee-name (car personnel-files)))
>        (if record
>            record
>            (find-employee-record (cdr personnel-files))))))
> ```

d. When Insatiable takes over a new company, what changes must be made in order to incorporate the new personnel information into the central system?

> You would have to add a new "row" to the type table, such as below:
>
> ```scheme
> (put 'new-company-file 'get-record
>     ; procedure to access new company's ADT here
>     )
> (put 'new-company-file 'get-salary
>     ; procedure to access new company's ADT here
>     )
> ```

**SICP Exercise 2.75** -
**SICP Exercise 2.76** -
**SICP Exercise 2.77** -
**SICP Exercise 2.79** -
**SICP Exercise 2.80** -
**SICP Exercise 2.81** -
**SICP Exercise 2.83** -

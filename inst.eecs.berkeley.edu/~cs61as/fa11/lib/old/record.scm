;   Record.scm
;   Library of Record / Table Management Functions.

;   **********************************************************************

;   M-Operator.
;   Extract the operator from an expression.
;   Parameters  :   Expression - The expression being evaluated.
;   Returns     :   The operator of an expression

(define (m-operator expression) (car expression))

;   **********************************************************************

;   M-Operands.
;   Extract the operands from an expression.
;   Parameters  :   Expression - The expression being evaluated.
;   Returns     :   The operands of an expression

(define (m-operands expression) (cdr expression))

;   **********************************************************************

;   Record.
;   Abstractly, a record is an association between a name and the object
;   it represents. You can use it to bind the name of a primitive operator
;   with the underlying representation of that operator as illustrated
;   below.
;   In this implementation a record is represented as a pair whose :
;   1.  Car is the name of the record.
;   2.  Cdr is the value bound to the record.

;    - -
;   | | |
;    - -
;    | |
;   '+ [#+proc]

;   **********************************************************************

;   M-Make-Record.
;   Construct a record from its constituent parts.
;   Parameters  :   Name - The record name.
;               :   Value - The value represented by the record.
;   Returns     :   A record.

(define (m-make-record name value) (cons name value))

;   **********************************************************************

;   M-Key.
;   Return a record key.
;   Parameters  :   Record
;   Returns     :   The record key.

(define (m-key record) (car record))

;   **********************************************************************

;   M-Data.
;   Return a record value.
;   Parameters  :   Record
;   Returns     :   The record data.

(define (m-data record) (cdr record))

;   **********************************************************************

;   M-Record?
;   Indicate whether an object is a record.
;   Parameters  :   Record - The object being evaluated.
;   Returns     :   #T if the object being evaluated is a record.
;               :   #F if the object being evaluated is not a record.

(define (m-record? record) (pair? record))

;   **********************************************************************

;   M-Record-Name.
;   Extract the name of a record from the record.
;   Parameters  :   Record - The record being evaluated.
;   Returns     :   The name of the record.

(define (m-record-name record)
    (if (m-record? record)
        (m-key record)
        (m-stop "Invalid Record" "M-Record-Name" record)))

;   **********************************************************************

;   M-Record-Value.
;   Extract the value of a record from the record.
;   Parameters  :   Record - The record being evaluated.
;   Returns     :   The value of the record.

(define (m-record-value record)
    (if (m-record? record)
        (m-data record)
        (m-stop "Invalid Record" "M-Record-Value" record)))

;   **********************************************************************

;   Table.
;   Abstractly, a table is a database; a collection of records.
;   In this implementation a table is represented as a list, whose :
;   1.  Car is the word 'Table.
;   2.  Cdr is a list of records.

;    - -   - -               - -               - -               - -
;   | | |-| | |-------------| | |-------------| | |-------------| |/|
;    - -   - -               - -               - -               - -
;    |     |                 |                 |                 |
;   'Table - -               - -               - -               - -
;         | | |             | | |             | | |             | | |
;          - -               - -               - -               - -
;          | |               | |               | |               | |
;         '+ [#+proc]       '- [#-proc]       '* [#*proc]       '/ [#/proc]

;   We will use the table abstraction to represent environments in our
;   interpreter.

;   **********************************************************************

;   M-Make-Table.
;   Construct an empty table.
;   Parameters  :   None.
;   Returns     :   A record.

(define (m-make-table) (list 'Table))

;   **********************************************************************

;   M-table?
;   Indicate whether an object is a table.
;   Parameters  :   Table - The object being evaluated.
;   Returns     :   #T if the object being evaluated is a table.
;               :   #F if the object being evaluated is not a table.

(define (m-table? table)
    (and
        (list? table)
        (equal? (car table) 'Table)))

;   **********************************************************************

;   M-Get-Record-Anchor
;   Find a record in a table whose key matches a designated key, returning
;   the cons cell to which it is attached.
;   Parameters  :   Table - The table being scanned.
;               :   Key - The key being sought.
;   Returns     :   The node to which the matching record is attached.
;               :   #F if no matching record can be found.

                                        ; Scan the records in the
                                        ; table for the first record
                                        ; whose key matches the
                                        ; designated key.

(define (m-get-record-anchor table key)
    (define (get-list lst)
        (cond                           ; If at the end of the lst,
            ((equal? lst '()) #F)       ; return an empty record.
            ((equal? (cdr lst) '()) #F)

                                        ; If the keys match, return the
                                        ; corresponding record.

            ((equal? key (m-record-name (cadr lst))) lst)

                                        ; Otherwise, search the rest of
                                        ; the lst.

            (else (get-list (cdr lst)))))

        (if                             ; Validate the table.
            (not (m-table? table))
                (m-stop "Invalid Table" "M-Get-Record Anchor" table)
            (get-list table)))

;   **********************************************************************

;   M-Get-Record.
;   Find a record in a table whose key matches a designated key.
;   Parameters  :   Table - The table being scanned.
;               :   Key - The key being sought.
;   Returns     :   The matching record if one can be found.
;               :   #F if no matching record can be found.

(define (m-get-record table key)

                                        ; Scan the records in the
                                        ; table for the first record
                                        ; whose key matches the
                                        ; designated key.

    (define anchor (m-get-record-anchor table key))

    (if (not anchor)
        #F
        (cadr anchor)))

;   **********************************************************************

;   M-Put-Record.
;   Put a record in a table.
;   Parameters  :   Table - The table into which the record is being
;               :   placed.
;               :   Record - The record being placed in the table.
;   Returns     :   Nothing.
;   Side Effect :   This procedure alters the table received in its
;               :   parameter list.

(define (m-put-record table record)

    (cond                               ; Validate the parameters.

        ((not (m-table? table))
            (m-stop "Invalid Table" "M-Put-Record" table))
        ((not (m-record? record))
            (m-stop "Invalid Record" "M-Put-Record" record))
        (else
            (set-cdr! table (m-make-record record (cdr table))))))



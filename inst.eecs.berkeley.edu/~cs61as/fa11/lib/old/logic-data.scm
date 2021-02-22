;;; The Itsey Bitsey Machine Corporation's personnel data base

;;; aa adds a rule or assertion to the database

(define (aa query)
  (add-rule-or-assertion!
    (add-assertion-body
      (query-syntax-process (list 'assert! query)))))

(aa '(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
(aa '(job (Bitdiddle Ben) (computer wizard)))
(aa '(salary (Bitdiddle Ben) 40000))

(aa '(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(aa '(job (Hacker Alyssa P) (computer programmer)))
(aa '(salary (Hacker Alyssa P) 35000))
(aa '(supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(aa '(address (Fect Cy D) (Cambridge (Ames Street) 3)))
(aa '(job (Fect Cy D) (computer programmer)))
(aa '(salary (Fect Cy D) 32000))
(aa '(supervisor (Fect Cy D) (Bitdiddle Ben)))

(aa '(address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(aa '(job (Tweakit Lem E) (computer technician)))
(aa '(salary (Tweakit Lem E) 15000))
(aa '(supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(aa '(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(aa '(job (Reasoner Louis) (computer programmer trainee)))
(aa '(salary (Reasoner Louis) 20000))
(aa '(supervisor (Reasoner Louis) (Hacker Alyssa P)))

(aa '(supervisor (Bitdiddle Ben) (Warbucks Oliver)))

(aa '(address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(aa '(job (Warbucks Oliver) (administration big wheel)))
(aa '(salary (Warbucks Oliver) 100000))

(aa '(address (Scrooge Eben) (Weston (Shady Lane) 10)))
(aa '(job (Scrooge Eben) (accounting chief accountant)))
(aa '(salary (Scrooge Eben) 69000))
(aa '(supervisor (Scrooge Eben) (Warbucks Oliver)))

(aa '(address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(aa '(job (Cratchet Robert) (accounting scrivener)))
(aa '(salary (Cratchet Robert) 12000))
(aa '(supervisor (Cratchet Robert) (Scrooge Eben)))

(aa '(address (Forrest Rosemary) (Slumerville (Onion Square) 5)))
(aa '(job (Forrest Rosemary) (administration secretary)))
(aa '(salary (Forrest Rosemary) 15000))
(aa '(supervisor (Forrest Rosemary) (Warbucks Oliver)))

(aa '(can-do-job (computer wizard) (computer programmer)))
(aa '(can-do-job (computer wizard) (computer technician)))

(aa '(can-do-job (computer programmer)
            (computer programmer trainee)))

(aa '(can-do-job (administration secretary)
            (administration big wheel)))

;;; end of data-base assertions

;;; Some rules

;;; This version of lives-near is from the text
;; commented out by Trey b/c we have the other version below
;(aa '(rule (lives-near ?person-1 ?person-2)
;      (and (address ?person-1 (?town . ?rest-1))
;           (address ?person-2 (?town . ?rest-2))
;           (not (lisp-value equal? ?person-1 ?person-2)))))

;;; This improved version of lives-near is not in the text
;;; (see instructor's manual)
(aa '(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2)))))

;;; This is not in the text (see instructor's manual)
(aa '(rule (same ?x ?x)))

(aa '(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager))))

(aa '(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss)))))

(aa '(rule (append-to-form () ?y ?y)))

(aa '(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z)))

;;; Exercise 4.33

(aa '(son Adam Cain))
(aa '(son Cain Enoch))
(aa '(son Enoch Irad))
(aa '(son Irad Mehujael))
(aa '(son Mehujael Methushael))
(aa '(son Methushael Lamech))
(aa '(wife Lamech Ada))
(aa '(son Ada Jabal))
(aa '(son Ada Jubal))

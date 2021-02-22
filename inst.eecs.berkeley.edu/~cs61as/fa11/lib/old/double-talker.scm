(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (se (usual 'say stuff) (ask self 'repeat)))) 



(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (se stuff stuff)))


(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (usual 'say (se stuff stuff))))

; Exercise 4.69: Beginning with the data base and the rules you formulated in Exercise 4.63, devise a rule for adding “greats” to a grandson relationship. This should enable the system to deduce that Irad is the great-grandson of Adam, or that Jabal and Jubal are the great-great-great-great-great-grandsons of Adam. (Hint: Represent the fact about Irad, for example, as ((great grandson) Adam Irad). Write rules that determine if a list ends in the word grandson. Use this to express a rule that allows one to derive the relationship ((great . ?rel) ?x ?y), where ?rel is a list ending in grandson.) Check your rules on queries such as ((great grandson) ?g ?ggs) and (?relationship Adam Irad).
(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")

(inssert_to_init '(
  (son Adam Cain) (son Cain Enoch)
  (son Enoch Irad) (son Irad Mehujael)
  (son Mehujael Methushael)
  (son Methushael Lamech)
  (wife Lamech Ada) (son Ada Jabal)
  (son Ada Jubal)
  
  (rule (grandson ?a ?c)
        (and (son ?a ?b)
             (son ?b ?c)))
  (rule (son ?a ?c)
        (and (wife ?a ?b)
              (son ?b ?c)))

  (rule (last-grandson? ?a)
    (or 
        (same ?a (grandson))
        (and (same ?a (?a1 . ?a2))
             (last-grandson? ?a2))))


  (rule ((great grandson) ?x ?y)
    (and (grandson ?x ?d)
    (son ?d ?y)))

  (rule ((great . ?rel) ?x ?y)
    (and 
    (last-grandson? ?rel)
    (son ?y0 ?y)
    (?rel ?x ?y0)))))
(inqu'(
  (last-grandson? (great great grandson))
  (son ?x ?y)
  ((great grandson) ?x ?y)
  ((great great great great great grandson) ?x ?y)
))


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.
; 'done


; (last-grandson? (great great grandson))


; (son Adam Cain)
; (son Cain Enoch)
; (son Enoch Irad)
; (son Irad Mehujael)
; (son Mehujael Methushael)
; (son Methushael Lamech)
; (son Ada Jabal)
; (son Ada Jubal)
; (son Lamech Jabal)
; (son Lamech Jubal)


; ((great grandson) Adam Irad)
; ((great grandson) Cain Mehujael)
; ((great grandson) Enoch Methushael)
; ((great grandson) Irad Lamech)
; ((great grandson) Mehujael Jabal)
; ((great grandson) Mehujael Jubal)


; ((great great great great great grandson) Adam Jabal)
; ((great great great great great grandson) Adam Jubal)
; > 
; Exercise 4.58: Define a rule that says that a person is a “big shot” in a division if the person works in the division but does not have a supervisor who works in the division.
(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")

(iqe 
 '(assert! 
   (rule (bigshot ?p1)
         (and (job ?p1 (?div1 . ?x1))
              (or (not (supervisor ?p1 ?p2))
                  (and 
                   (supervisor ?p1 ?p2)
                   (job ?p2 (?div2 . ?x2))
                   (not (same ?div1 ?div2)))))))
 '(bigshot ?p1)

 )


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.

; Assertion added to data base.

; (bigshot (Warbucks Oliver))
; (bigshot (Scrooge Eben))
; (bigshot (Bitdiddle Ben))
; > 
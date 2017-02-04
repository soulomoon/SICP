; Exercise 4.57: Define a rule that says that person 1 can replace person 2 if either person 1 does the same job as person 2 or someone who does person 1’s job can also do person 2’s job, and if person 1 and person 2 are not the same person. Using your rule, give queries that find the following:

; all people who can replace Cy D. Fect;
; all people who can replace someone who is being paid more than they are, together with the two salaries.
(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")


(iqe
 '(assert! 
   (rule (replace ?p1 ?p2) 
         (and (and (job ?p1 ?dj1)
                   (job ?p2 ?dj2))
              (not (same ?p1 ?p2))
              (or (same ?dj1 ?dj2)
                  (can-do-job ?dj1 ?dj2)))))
 '(replace ?p (Fect Cy D))
 
 '(and (replace ?p1 ?p2)
       (salary ?p1 ?a1)
       (salary ?p2 ?a2)
       (lisp-value < ?a1 ?a2)
       )
 
 )

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.

; Assertion added to data base.

; (replace (Hacker Alyssa P) (Fect Cy D))
; (replace (Bitdiddle Ben) (Fect Cy D))


; (and (replace (Fect Cy D) (Hacker Alyssa P)) (salary (Fect Cy D) 35000) (salary (Hacker Alyssa P) 40000) (lisp-value < 35000 40000))
; (and (replace (Aull DeWitt) (Warbucks Oliver)) (salary (Aull DeWitt) 25000) (salary (Warbucks Oliver) 150000) (lisp-value < 25000 150000))
; > 
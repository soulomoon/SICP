; Exercise 4.57: Define a rule that says that person 1 can replace person 2 if either person 1 does the same job as person 2 or someone who does person 1’s job can also do person 2’s job, and if person 1 and person 2 are not the same person. Using your rule, give queries that find the following:

; all people who can replace Cy D. Fect;
; all people who can replace someone who is being paid more than they are, together with the two salaries.
(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")


; add iqe so I can manage to interpret a bunch of code at one time
(iq 
 '(assert! 
   (rule (replace ?p1 ?p2) 
         (and (and (job ?p1 ?dj1)
                   (job ?p2 ?dj2))
              (not (same ?p1 ?p2))
              (or (same ?dj1 ?dj2)
                  (can-do-job ?dj1 ?dj2))))))

(iq 
 '(replace ?p (Fect Cy D)))

(iq
'())


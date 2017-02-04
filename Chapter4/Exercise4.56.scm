; Exercise 4.56: Formulate compound queries that retrieve the following information:

; the names of all people who are supervised by Ben Bitdiddle, together with their addresses;
; all people whose salary is less than Ben Bitdiddle’s, together with their salary and Ben Bitdiddle’s salary;
; all people who are supervised by someone who is not in the computer division, together with the supervisor’s name and job.

(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")

(iq '(and (supervisor ?person (Bitdiddle Ben)) (address ?person ?ad)))

(iq '(and (salary ?p ?amout) 
          (and (salary (Bitdiddle Ben) ?amout1) 
               (lisp-value > ?amout1 ?amout))))

(iq 
'(and (supervisor ?s ?sup)
      (job ?sup ?j2)
      (not (job ?sup (computer . ?j)))))


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.
; 'done


; (and (supervisor (Tweakit Lem E) (Bitdiddle Ben)) (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
; (and (supervisor (Fect Cy D) (Bitdiddle Ben)) (address (Fect Cy D) (Cambridge (Ames Street) 3)))
; (and (supervisor (Hacker Alyssa P) (Bitdiddle Ben)) (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))


; (and (salary (Aull DeWitt) 25000) (and (salary (Bitdiddle Ben) 60000) (lisp-value > 60000 25000)))
; (and (salary (Cratchet Robert) 18000) (and (salary (Bitdiddle Ben) 60000) (lisp-value > 60000 18000)))
; (and (salary (Reasoner Louis) 30000) (and (salary (Bitdiddle Ben) 60000) (lisp-value > 60000 30000)))
; (and (salary (Tweakit Lem E) 25000) (and (salary (Bitdiddle Ben) 60000) (lisp-value > 60000 25000)))
; (and (salary (Fect Cy D) 35000) (and (salary (Bitdiddle Ben) 60000) (lisp-value > 60000 35000)))
; (and (salary (Hacker Alyssa P) 40000) (and (salary (Bitdiddle Ben) 60000) (lisp-value > 60000 40000)))


; (and (supervisor (Aull DeWitt) (Warbucks Oliver)) (job (Warbucks Oliver) (administration big wheel)) (not (job (Warbucks Oliver) (computer . ?j))))
; (and (supervisor (Cratchet Robert) (Scrooge Eben)) (job (Scrooge Eben) (accounting chief accountant)) (not (job (Scrooge Eben) (computer . ?j))))
; (and (supervisor (Scrooge Eben) (Warbucks Oliver)) (job (Warbucks Oliver) (administration big wheel)) (not (job (Warbucks Oliver) (computer . ?j))))
; (and (supervisor (Bitdiddle Ben) (Warbucks Oliver)) (job (Warbucks Oliver) (administration big wheel)) (not (job (Warbucks Oliver) (computer . ?j))))
; > 
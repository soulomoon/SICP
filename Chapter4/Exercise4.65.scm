; Exercise 4.65: Cy D. Fect, looking forward to the day when he will rise in the organization, gives a query to find all the wheels (using the wheel rule of 4.4.1):

; (wheel ?who)
; To his surprise, the system responds

; ;;; Query results:
; (wheel (Warbucks Oliver))
; (wheel (Bitdiddle Ben))
; (wheel (Warbucks Oliver))
; (wheel (Warbucks Oliver))
; (wheel (Warbucks Oliver))
; Why is Oliver Warbucks listed four times?

(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")

(inqu '(
        (wheel ?x)
        (and (supervisor ?middle-manager 
                         ?person)
             (supervisor ?x ?middle-manager))
        ))

; because he has four ways of being a wheel as show below

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.


; (wheel (Warbucks Oliver))
; (wheel (Warbucks Oliver))
; (wheel (Bitdiddle Ben))
; (wheel (Warbucks Oliver))
; (wheel (Warbucks Oliver))


; (and (supervisor (Scrooge Eben) (Warbucks Oliver)) (supervisor (Cratchet Robert) (Scrooge Eben)))
; (and (supervisor (Bitdiddle Ben) (Warbucks Oliver)) (supervisor (Tweakit Lem E) (Bitdiddle Ben)))
; (and (supervisor (Hacker Alyssa P) (Bitdiddle Ben)) (supervisor (Reasoner Louis) (Hacker Alyssa P)))
; (and (supervisor (Bitdiddle Ben) (Warbucks Oliver)) (supervisor (Fect Cy D) (Bitdiddle Ben)))
; (and (supervisor (Bitdiddle Ben) (Warbucks Oliver)) (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))
; > 
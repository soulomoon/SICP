; Exercise 4.55: Give simple queries that retrieve the following information from the data base:

; all people supervised by Ben Bitdiddle;
; the names and jobs of all people in the accounting division;
; the names and addresses of all people who live in Slumerville.


; loading is from "https://wizardbook.wordpress.com/2011/01/24/exercise-4-55/"
; I use iq as interpret-query.
; thankx for Barry Allison that I could continue my exercise
(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")


(iq '(supervisor ?name (Bitdiddle Ben)))
(iq '(job ?name (accounting . ?y)))
(iq '(address ?name (Slumerville . ?y)))


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.
; 'done


; (supervisor (Tweakit Lem E) (Bitdiddle Ben))
; (supervisor (Fect Cy D) (Bitdiddle Ben))
; (supervisor (Hacker Alyssa P) (Bitdiddle Ben))


; (job (Cratchet Robert) (accounting scrivener))
; (job (Scrooge Eben) (accounting chief accountant))


; (address (Aull DeWitt) (Slumerville (Onion Square) 5))
; (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
; (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
; > 
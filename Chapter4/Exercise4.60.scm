; Exercise 4.60: By giving the query

; (lives-near ?person (Hacker Alyssa P))
; Alyssa P. Hacker is able to find people who live near her, with whom she can ride to work. On the other hand, when she tries to find all pairs of people who live near each other by querying

; (lives-near ?person-1 ?person-2)
; she notices that each pair of people who live near each other is listed twice; for example,

; (lives-near (Hacker Alyssa P) (Fect Cy D))
; (lives-near (Fect Cy D) (Hacker Alyssa P))
; Why does this happen? Is there a way to find a list of people who live near each other, in which each pair appears only once? Explain.

; 1 because the variable is order-sensitive
; 2 yes, although query just filter and not keep track of what it is still there, but you would have to make it order-insensitive.
; which you have to find a way to compare which variable is bigger than the other in some way.

(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")

; solution is found from "https://wizardbook.wordpress.com/2011/01/29/exercise-4-60/"

(define (accumulate p init l)
  (if (null? l)
      init
      (accumulate p (p (car l)) (cdr l)))) 
 
(define (symbol-list->string p)
  (accumulate string-append "" (map symbol->string p)))
 
(define (symbol-list>? person1 person2)
  (string>? (symbol-list->string person1)
            (symbol-list->string person2)))


(set! primitive-procedures (cons (list 'symbol-list>? symbol-list>?) primitive-procedures))
(define user-initial-environment (setup-environment))

(iqe
 '(assert!
   (rule (lives-near-u ?person-1 ?person-2)
         (and (address ?person-1 (?town . ?rest-1))
              (address ?person-2 (?town . ?rest-2))
              (lisp-value symbol-list>? ?person-1 ?person-2))))

 '(lives-near-u ?y ?x)
 )

;  Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.

; Assertion added to data base.

; (lives-near-u (Aull DeWitt) (Bitdiddle Ben))
; (lives-near-u (Reasoner Louis) (Aull DeWitt))
; (lives-near-u (Reasoner Louis) (Bitdiddle Ben))
; (lives-near-u (Hacker Alyssa P) (Fect Cy D))
; > 
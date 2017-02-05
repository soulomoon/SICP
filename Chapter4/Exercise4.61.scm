
; Exercise 4.61: The following rules implement a next-to relation that finds adjacent elements of a list:

; (rule (?x next-to ?y in (?x ?y . ?u)))
; (rule (?x next-to ?y in (?v . ?z))
;       (?x next-to ?y in ?z))
; What will the response be to the following queries?

; (?x next-to ?y in (1 (2 3) 4))
; (?x next-to 1 in (2 1 3 1))

(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")

; That I have to put the rule in setup in order for it to work

(define (inssert_to_init rule_list)
  (for-each (lambda (rule) (set! microshaft-data-base (cons rule microshaft-data-base))) rule_list))

(inssert_to_init '(
  (rule (?x next-to ?y in (?x ?y . ?u)))
  (rule (?x next-to ?y in (?v . ?z))
    (?x next-to ?y in ?z))
))
(setup-data-base)

(inqu'(

(?x next-to ?y in (1 (2 3) 4))
(?x next-to 1 in (2 1 3 1))

))
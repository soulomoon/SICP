; Exercise 4.5: Scheme allows an additional syntax for cond clauses, (⟨test⟩ => ⟨recipient⟩). If ⟨test⟩ evaluates to a true value, then ⟨recipient⟩ is evaluated. Its value must be a procedure of one argument; this procedure is then invoked on the value of the ⟨test⟩, and the result is returned as the value of the cond expression. For example

; (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;       (else false))
; returns 2. Modify the handling of cond so that it supports this extended syntax.
(load "/home/soulomoon/git/SICP/Chapter4/ex4.03.rkt")

(define (cond? exp) 
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause) 
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (cond-=>-clause? clause)
  (if (and (cond-actions clause) 
           (eq? (car (cond-actions clause)) '=>))
      true
      false))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp 
                 (cond-actions first))
                (error "ELSE clause isn't 
                        last: COND->IF"
                       clauses))
            (if (cond-=>-clause? first)
              (make-if (cond-predicate first)
                      (list (cadr (cond-actions first)) 
                            (cond-predicate first))
                      (expand-clauses 
                        rest))
              (make-if (cond-predicate first)
                      (sequence->exp 
                        (cond-actions first))
                      (expand-clauses 
                        rest))
                        )))))

(interpret
 '(cond ((assoc 'b '((a 1) (b 2))) => cadr) (else false)))
            

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 2
; > 
; Translation procedures:
;
; ex4.03.rkt is using the neil/sicp module which interacts in a strange way
; with rackets normal lists and mutable lists. 
; These procedures translate values between 
; the evaluator only works with expressions that are mutable lists so 
; exp->mlist takes a normal expression and creates a mutable list from it
#lang racket/base
(require racket/mpair)
(require rackunit
         "ex4.03.rkt")
(define (exp->mlist exp)
  (if (pair? exp)
      (foldr (lambda (x rest)
               (if (pair? x)
                   (mcons (exp->mlist x) rest)
                   (mcons x rest)))
             null exp)
      exp))
(define (mlist->exp exp)
  (if (mlist? exp)
      (mlist->list exp)
      exp))
  
(define (run-interpreter exp)
  (mlist->exp (interpret (exp->mlist exp))))


(provide run-interpreter)
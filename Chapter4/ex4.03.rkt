; #lang racket/base
; (require (except-in (planet neil/sicp)
;                     make-frame))
; ;; Syntax table procedures for meta-circular evaluator
; (require "ex4.table.rkt")
(load "/home/soulomoon/git/SICP/Chapter4/ex4.table.rkt")
; this version of the evaluator includes these expression types:
;  self-evaluating 
;  variable
;  quote 
;  define 
;  set! 
;  if
;  lambda 
;  begin 
;  cond 
;  application? 

(define (eval# exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get-syntax (type-tag exp)) ((get-syntax (type-tag exp)) exp env))
        ((application? exp)
         (apply# (eval# (operator exp) env)
                 (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define apply-in-underlying-scheme apply)

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (apply# procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

; Procedure arguments

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval# (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; Quoted data

(define (eval-quoted exp env)
  (text-of-quotation exp))

; Conditionals

(define (eval-if exp env)
  (if (true? (eval# (if-predicate exp) env))
      (eval# (if-consequent exp) env)
      (eval# (if-alternative exp) env)))

; Sequences

(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))


(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval# (first-exp exps) env))
        (else (eval# (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; Assignments and definitions

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval# (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval# (definition-value exp) env)
    env)
  'ok)

; Anonymous procedures

(define (eval-lambda exp env)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))

; Cond Case dispatch

(define (eval-cond exp env)
  (eval# (cond->if exp) env))


; ¤ The only self-evaluating items are numbers and strings:

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; ¤ Variables are represented by symbols:

(define (variable? exp) (symbol? exp))

; ¤ Quotations have the form (quote <text-of-quotation>)

;  The reader in DrScheme evaluates '(quote x) and tranforms it into 'x before passing it to eval# 
;   so we are looking here for any other atom (not a number or string as they have been dealt with) 
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

; Quoted? is defined in terms of the procedure tagged-list?, which identifies lists beginning with a designated symbol:

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; ¤ Assignments have the form (set! <var> <value>):

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; ¤ Definitions have the form
;(define <var> <value>)
;or the form
;(define (<var> <parameter1> ... <parametern>)
;  <body>) which is syntactic sugar for
;define <var>
;  (lambda (<parameter1> ... <parametern>)
;    <body>))

; The corresponding syntax procedures are the following:

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

;¤ Lambda expressions are lists that begin with the symbol lambda:

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

; We also provide a constructor for lambda expressions, which is used by definition-value, above:

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; ¤ Conditionals begin with if and have a predicate, a consequent, and an (optional) alternative. 
; If the expression has no alternative part, we provide false as the alternative.10

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

; We also provide a constructor for if expressions, to be used by cond->if to transform cond expressions into if expressions:

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; ¤ Begin packages a sequence of expressions into a single expression. We include syntax operations on begin expressions to extract the actual sequence from the begin expression, as well as selectors that return the first expression and the rest of the expressions in the sequence.11

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

; We also include a constructor sequence->exp (for use by cond->if) that transforms a sequence into a single expression, using begin if necessary:
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

; ¤ A procedure application is any compound expression that is not one of the above expression types. The car of the expression is the operator, and the cdr is the list of operands:

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; Derived expressions

; Some special forms in our language can be defined in terms of expressions involving other special forms, 
; rather than being implemented directly. One example is cond, which can be implemented as a nest of if expressions. 

; Implementing the evaluation of cond in this way simplifies the evaluator because it reduces 
; the number of special forms for which the evaluation process must be explicitly specified.

; We include syntax procedures that extract the parts of a cond expression, 
; and a procedure cond->if that transforms cond expressions into if expressions. 
; A case analysis begins with cond and has a list of predicate-action clauses. 
; A clause is an else clause if its predicate is the symbol else.12

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

; For conditionals, we accept anything to be true that is not the explicit false object.
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

; Representing procedures
; To handle primitives, we assume that we have available the following procedures:
;    * (apply-primitive-procedure <proc> <args>)
;    * (primitive-procedure? <proc>)
; Compound procedures are constructed from parameters, procedure bodies, and environments using the constructor make-procedure:
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

; Operations on Environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; Each frame of an environment is represented as a pair of lists: 
; a list of the variables bound in that frame and 
; a list of the associated values
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; To look up a variable in an environment, we scan the list of variables in the first frame. 
; If we find the desired variable, we return the corresponding element in the list of values. 
; If we do not find the variable in the current frame, we search the enclosing environment, and so on. 
; If we reach the empty environment, we signal an ``unbound variable'' error.

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (install-syntax)
  (put-syntax! 'quote eval-quoted) 
  (put-syntax! 'define eval-definition) 
  (put-syntax! 'set! eval-definition) 
  (put-syntax! 'if eval-if) 
  (put-syntax! 'lambda eval-lambda) 
  (put-syntax! 'begin eval-begin) 
  (put-syntax! 'cond eval-cond) 
  'done)
(install-syntax)


; 4.1.4  Running the Evaluator as a Program
(define primitive-procedures
  (list (list '* *)
        (list '+ +)
        (list '- -)
        (list '/ /)
        (list '< <)
        (list '<= <=)
        (list '= =)
        (list '> >)
        (list '>= >=)
        (list 'abs abs)
        (list 'acos acos)
        (list 'append append)
        (list 'asin asin)
        (list 'assoc assoc)
        (list 'assq assq)
        (list 'assv assv)
        (list 'atan atan)
        (list 'boolean? boolean?)
        (list 'caaaar caaaar)
        (list 'caaadr caaadr)
        (list 'caaar caaar)
        (list 'caadar caadar)
        (list 'caaddr caaddr)
        (list 'caadr caadr)
        (list 'caar caar)
        (list 'cadaar cadaar)
        (list 'cadadr cadadr)
        (list 'cadar cadar)
        (list 'caddar caddar)
        (list 'cadddr cadddr)
        (list 'caddr caddr)
        (list 'cadr cadr)
        (list 'car car)
        (list 'cdaaar cdaaar)
        (list 'cdaadr cdaadr)
        (list 'cdaar cdaar)
        (list 'cdadar cdadar)
        (list 'cdaddr cdaddr)
        (list 'cdadr cdadr)
        (list 'cdar cdar)
        (list 'cddaar cddaar)
        (list 'cddadr cddadr)
        (list 'cddar cddar)
        (list 'cdddar cdddar)
        (list 'cddddr cddddr)
        (list 'cdddr cdddr)
        (list 'cddr cddr)
        (list 'cdr cdr)
        (list 'ceiling ceiling)
        (list 'char->integer char->integer)
        (list 'char-alphabetic? char-alphabetic?)
        (list 'char-ci<=? char-ci<=?)
        (list 'char-ci=? char-ci=?)
        (list 'char-ci>=? char-ci>=?)
        (list 'char-ci>? char-ci>?)
        (list 'char-downcase char-downcase)
        (list 'char-lower-case? char-lower-case?)
        (list 'char-numeric? char-numeric?)
        (list 'char-upcase char-upcase)
        (list 'char-upper-case? char-upper-case?)
        (list 'char-whitespace? char-whitespace?)
        (list 'char<=? char<=?)
        (list 'char=? char=?)
        (list 'char>=? char>=?)
        (list 'char>? char>?)
        (list 'char? char?)
        (list 'complex? complex?)
        (list 'cons cons)
        (list 'cos cos)
        (list 'display display)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'eqv? eqv?)
        (list 'eval eval)
        (list 'even? even?)
        (list 'exact? exact?)
        (list 'exp exp)
        (list 'expt expt)
        (list 'floor floor)
        (list 'for-each for-each)
        (list 'force force)
        (list 'gcd gcd)
        (list 'inexact? inexact?)
        (list 'integer->char integer->char)
        (list 'integer? integer?)
        (list 'lcm lcm)
        (list 'length length)
        (list 'list list)
        (list 'list->string list->string)
        (list 'list->vector list->vector)
        (list 'list-ref list-ref)
        (list 'list-tail list-tail)
        (list 'list? list?)
        (list 'log log)
        (list 'make-string make-string)
        (list 'make-vector make-vector)
        (list 'map map)
        (list 'max max)
        (list 'member member)
        (list 'memq memq)
        (list 'memv memv)
        (list 'min min)
        (list 'modulo modulo)
        (list 'negative? negative?)
        (list 'newline newline)
        (list 'not not)
        (list 'null? null?)
        (list 'number->string number->string)
        (list 'number? number?)
        (list 'odd? odd?)
        (list 'pair? pair?)
        (list 'positive? positive?)
        (list 'quotient quotient)
        (list 'rational? rational?)
        (list 'real? real?)
        (list 'remainder remainder)
        (list 'reverse reverse)
        (list 'round round)
        (list 'sin sin)
        (list 'sqrt sqrt)
        (list 'string string)
        (list 'string->list string->list)
        (list 'string->number string->number)
        (list 'string->symbol string->symbol)
        (list 'string-append string-append)
        (list 'string-ci<=? string-ci<=?)
        (list 'string-ci=? string-ci=?)
        (list 'string-ci>=? string-ci>=?)
        (list 'string-ci>? string-ci>?)
        (list 'string-copy string-copy)
        (list 'string-fill! string-fill!)
        (list 'string-length string-length)
        (list 'string-ref string-ref)
        (list 'string-set! string-set!)
        (list 'string<=? string<=?)
        (list 'string string)
        (list 'string=? string=?)
        (list 'string>=? string>=?)
        (list 'string>? string>?)
        (list 'string? string?)
        (list 'substring substring)
        (list 'symbol->string symbol->string)
        (list 'tan tan)
        (list 'truncate truncate)
        (list 'vector vector)
        (list 'vector->list vector->list)
        (list 'vector-fill! vector-fill!)
        (list 'vector-length vector-length)
        (list 'vector-ref vector-ref)
        (list 'vector-set! vector-set!)
        (list 'vector? vector?)
        (list 'write write)
        (list 'write-char write-char)
        (list 'zero? zero?) 
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


(define the-global-environment (setup-environment))

(define (interpret exp)
  (eval# exp the-global-environment))

;; Test procedures are run from a different file using rackunit 
; (provide interpret)

; Translation procedures:
;
; ex4.03.rkt is using the neil/sicp module which interacts in a strange way
; with rackets normal lists and mutable lists. 
; These procedures translate values between 
; the evaluator only works with expressions that are mutable lists so 
; exp->mlist takes a normal expression and creates a mutable list from it
; (require racket/mpair)
; (define (exp->mlist exp)
;   (if (pair? exp)
;       (foldr (lambda (x rest)
;                (if (pair? x)
;                    (mcons (exp->mlist x) rest)
;                    (mcons x rest)))
;              null exp)
;       exp))
; (define (mlist->exp exp)
;   (if (mlist? exp)
;       (mlist->list exp)
;       exp))
  
; (define (run-interpreter exp)
;   (mlist->exp (interpret (exp->mlist exp))))


; (provide run-interpreter)
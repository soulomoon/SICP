; #lang racket/base
; (require (except-in (planet neil/sicp)
;                     make-frame))
(#%require racket/base)
(#%require sicp)
; this version of the evaluator includes these expression types:
;  self-evaluating 
;  variable
;  quote 
;  define 
;  set! 
;  if
;  lambda 
;  begin 
;  cond + <test> => <recipient>
;  application? 
;  let

(define (eval# exp env) ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp)          (analyze-quoted exp))
        ((variable? exp)        (analyze-variable exp))
        ((assignment? exp)      (analyze-assignment exp))
        ((definition? exp)      (analyze-definition exp))
        ((if? exp)              (analyze-if exp))
        ((lambda? exp)          (analyze-lambda exp))
        ((begin? exp)           (analyze-sequence (begin-actions exp)))
        ((cond? exp)            (analyze (cond->if exp)))
        ((let? exp)             (analyze (let->combination exp)))
        ((application? exp)     (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp)))) 

(define (analyze-self-evaluating exp)
  (lambda (env) exp))    

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))    

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))  

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))  

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

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

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval# (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; ¤ The only self-evaluating items are numbers, strings and boolean values
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
        (else false)))

; To look up a variable in an environment, we scan the list of variables in the first frame. 
; If we find the desired variable, we return the corresponding element in the list of values. 
; If we do not find the variable in the current frame, we search the enclosing environment, and so on. 
; If we reach the empty environment, we signal an ``unbound variable'' error.

(define (variable? exp) (symbol? exp))

; Ex 4.14a raise an error if a variable is present but unassigned within a frame
(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (cond ((bound? frame var)      (frame-binding frame var))
                ((unassigned? frame var) (error "Unassigned variable" var))
                (else                    (env-loop (enclosing-environment env)))))))
  (env-loop env))

(define (remove-variable-value-universal! var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (cond ((bound? frame var)      (remove-binding-from-frame! frame var))
                ((unassigned? frame var) (remove-binding-from-frame! frame var))
                (else                    (env-loop (enclosing-environment env)))))))
  (env-loop env))

(define (remove-variable-value! var env)
  (let ((frame (first-frame env)))
    (if (bound? frame var)
        (remove-binding-from-frame! frame var)
        (error "Unbound variable" var))))

; Operations on Environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; Each frame of an environment is represented as a pair of lists: 
; a list of the variables bound in that frame and 
; a list of the associated values
(define unbound    '*unbound*)
(define unassigned '*unassigned*)

(define (frame-binding frame var)
  (hash-ref frame var unbound))

(define (bound? frame var)
  (not (eq? unbound
            (frame-binding frame var))))

(define (unassigned? frame var)
  (eq? unassigned
       (frame-binding frame var)))


(define (add-binding-to-frame! var val frame)
  (hash-set! frame var val))

(define (remove-binding-from-frame! frame var)
  (hash-remove! frame var))

(define (set-binding-in-frame! var val frame)
  (hash-set! frame var val))

(define (make-frame variables values)
  (let ((new-frame (make-hash)))
    (map (lambda (var val) 
           (add-binding-to-frame! var val new-frame ))
         variables 
         values)
    new-frame))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; Boolean expressions
(define (and? exp) (tagged-list? exp 'and))
(define (or?  exp) (tagged-list? exp 'or))

(define boolean-expression-list cdr)

(define (eval-and exp env)
  (define (eval-expression-list exp)
    (cond [(last-exp? exp) (eval# (first-exp exp) env)]
          [(eval# (first-exp exp) env) (eval-expression-list (rest-exps exp))]
          [else false]))
  (eval-expression-list (boolean-expression-list exp)))

(define (eval-or exp env)
  (eval# (or->if (boolean-expression-list exp) env) env))

(define (or->if exp env)
  (cond [(last-exp? exp) (eval# (first-exp exp) env)]
        [else (make-if (first-exp exp)
                       true
                       (or->if (rest-exps exp) env))]))

; Let forms
(define (let? exp)    (tagged-list? exp 'let))

(define (let-initials exp)   (map cadr (cadr exp)))
(define (let-parameters exp) (map car (cadr exp)))
(define (let-body exp)       (cddr exp))
(define named-let-identifier car)

(define (named-let? exp)
  (symbol? (cadr exp)))

(define (let->combination exp)
  (if (named-let? exp)
      (named-let->combination (cdr exp))
      (cons (make-lambda (let-parameters exp) 
                         (let-body exp))
            (let-initials exp))))

; A named let is equivalent to a procedure definition 
; followed by a single application of that procedure with the 
; initial values given by the let expression.
;
; exp should be the initial let expression stripped of the 'let symbol
;   this allows the same selection preocedures to be used without altering them.
(define (named-let->combination exp)
  (let ((procedure-name (named-let-identifier exp)))
    ; 2 expressions are needed so wrao them in a begin form
    (make-begin 
     (list
      ; define the procedure with the name given in the let expression
      (list 'define procedure-name 
            (make-lambda 
             (let-parameters exp) 
             (let-body exp)))
      ; apply the procedure with the initial values given by the let expression
      (cons procedure-name (let-initials exp))))))

; The following are all insatlled into the syntax table 
; and have the form (eval-proc exp env) with no requirements to delay
; argument evaluation (such as with if / cond)
; See (install) for the list of syntax expressions 

; Quoted data
(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))
(define (eval-quoted exp env)   (text-of-quotation exp))

; Definitions
(define (definition? exp) (tagged-list? exp 'define))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval# (definition-value exp) env)
    env)
  'ok)

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (if (bound? frame var)
        (set-binding-in-frame! var val frame)
        (add-binding-to-frame! var val frame))))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

; set!
(define (assignment? exp) (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))  
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval# (assignment-value exp) env)
                       env)
  'ok)  

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (if (bound? frame var)
              (set-binding-in-frame! var val frame)
              (env-loop (enclosing-environment env))))))    
  (env-loop env))  


; Removing bindings from the environment
(define (make-unbound? exp) (tagged-list? exp 'make-unbound!))

(define make-unbound-variable cadr)

(define (eval-make-unbound exp env)
  (let ((var (make-unbound-variable exp)))
    (if (symbol? var)
        (remove-variable-value! var env)
        (error "Variable expected -- EVAL-MAKE-UNBOUND" exp))))

; Conditionals

(define (if? exp) (tagged-list? exp 'if))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (if-predicate exp)   (cadr exp))
(define (if-consequent exp)  (caddr exp))
(define (if-alternative exp) (if (not (null? (cdddr exp)))
                                 (cadddr exp)
                                 'false))

(define (eval-if exp env)
  (if (true? (eval# (if-predicate exp) env))
      (eval# (if-consequent exp) env)
      (eval# (if-alternative exp) env)))

; Anonymous procedures
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp)       (cddr exp))

(define (eval-lambda exp env)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))

; Sequences
(define (begin? exp) (tagged-list? exp 'begin))

(define (make-begin seq) (cons 'begin seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (begin-actions exp) (cdr exp))
(define (last-exp? seq)     (null? (cdr seq)))
(define (first-exp seq)     (car seq))
(define (rest-exps seq)     (cdr seq))

(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval# (first-exp exps) env))
        (else (eval# (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; cond
(define (cond? exp) (tagged-list? exp 'cond))

(define (eval-cond exp env)
  (eval# (cond->if exp) env))

(define (make-cond-recipient clause predicate)
  (list (cond-recipient clause) predicate))

(define (cond-clauses exp)              (cdr exp))
(define (cond-else-clause? clause)      (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)         (car clause))
(define (cond-actions clause)           (cdr clause))
(define (cond-recipient clause)         (caddr clause))
(define (cond-recipient-clause? clause) (eq? (cadr clause) '=>))
(define (cond-consequent clause predicate)
  (if (cond-recipient-clause? clause)
      (make-cond-recipient clause predicate)
      (sequence->exp (cond-actions clause))))

; this checks against the 2 forms for cond clauses
; 1) ((pred-clauses) (value-clauses)) -> result is (value-clauses)
; 2) ((pred-clauses) => proc)         -> result is (proc v)

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
            (let ((predicate (cond-predicate first)))
              (make-if predicate
                       (cond-consequent first predicate)
                       (expand-clauses rest)))))))

; Loops
(define (while? exp) (tagged-list? exp 'while))

(define (eval-while exp env)
  (eval# (while->combination exp) env))

; while selectors
(define while-predicate cadr)
(define while-body      cddr)

; (while pred body) => 
; (begin (define (while-loop) 
;          (if pred body #f))
;          (while-loop))
(define (while->combination exp)
  (sequence->exp
   (append (make-while-definition exp)
           (while-invocation))))

(define (make-while-definition exp)
  (list (append '(define (while-loop))
                (make-while-body exp))))

(define (make-while-body exp)
  (list
   (make-if (while-predicate exp)
            (while->if-body exp)
            #f)))

(define (while->if-body exp)
  (sequence->exp (append (while-body exp)
                         (while-invocation))))

(define (while-invocation)
  (list '(while-loop)))

; ¤ A procedure application is any compound expression that is not one of the above expression types. The car of the expression is the operator, and the cdr is the list of operands:

(define (application? exp) (pair? exp))


; For conditionals, we accept anything to be true that is not the explicit false object.
(define (true? x)  (not (eq? x false)))
(define (false? x) (eq? x false))

; Representing procedures
; To handle primitives, we assume that we have available the following procedures:
;    * (apply-primitive-procedure <proc> <args>)
;    * (primitive-procedure? <proc>)
; Compound procedures are constructed from parameters, procedure bodies, and environments using the constructor make-procedure:
(define (compound-procedure? p)   (tagged-list? p 'procedure))
(define (procedure-parameters p)  (cadr p))
(define (procedure-body p)        (caddr p))
(define (procedure-environment p) (cadddr p))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))


(define (extract-declarations exp)
  (define (scan-iter body scan-complete)
    (cond ((null? body) null)
          ((definition? (car body))
           (if scan-complete
               (error "define cannot appear in an expression context - LAMBDA-DEFINES" exp)
               (cons (car body) 
                     (scan-iter (cdr body) #f))))
          (else (scan-iter (cdr body) #t))))
  (scan-iter exp #f))

(define (make-let-seq var-list body)
  (append (list 'let var-list) 
          body))

(define (make-let-unassigned define-list)
  (map (lambda (def)  
         (list (definition-variable def) 
               `(quote ,unassigned)))
       define-list))

(define (make-let-assigned variables values)
  (map (lambda (var val)  
         (list var val))
       variables
       values))

(define (make-set!-list variables values)
  (map (lambda (var val)  
         (list 'set! var val))
       variables
       values))


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
    (define-variable! 'true  true  initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

; Provide a simple method of evaluating a form
(define (interpret exp)
  (eval# exp the-global-environment))

; ;; Test procedures are run from a different file using rackunit 
; (provide interpret
;          eval#
;          apply#
;          setup-environment)



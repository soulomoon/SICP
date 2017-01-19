(load "/Users/soulomoon/git/SICP/Chapter4/prompt.scm")

(define (force-it-without-memo obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) 
                    (thunk-env obj))
      obj))
(define (delay-it exp env)
  ; ; (display "delay-it--------")(display exp)(newline )
  (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) 
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result
                (actual-value 
                 (thunk-exp obj)
                 (thunk-env obj))))
          ;  (display "force-it------")
          ;  (display (car obj))
          ;  (display (thunk-exp obj))(newline)
           (set-car! obj 'evaluated-thunk)
           ;; replace exp with its value:
           (set-car! (cdr obj) result) 
           ;; forget unneeded env:
           (set-cdr! (cdr obj) '()) 
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))





(define (eval# exp env)
  ; ; (display "eval#-----")(display exp)(newline )
  (define (eval-inner exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get-syntax (type-tag exp)) ((get-syntax (type-tag exp)) exp env))
        ((application? exp)
        (apply# (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))
  (let ((result (eval-inner exp env)))
  ; ; (display "eval#end-----")(display result)(newline )

      result
  )
)
(define (actual-value exp env)
  (force-it (eval# exp env)))

(define (apply# procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values 
           arguments 
           env)))  ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args 
            arguments 
            env)   ; changed
           (procedure-environment procedure))))
        (else (error "Unknown procedure 
                      type: APPLY" 
                     procedure))))
(define (list-of-arg-values exps env)

  (if (no-operands? exps)
      '()
      (cons (actual-value 
             (first-operand exps) 
             env)
            (list-of-arg-values 
             (rest-operands exps)
             env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it 
             (first-operand exps) 
             env)
            (list-of-delayed-args 
             (rest-operands exps)
             env))))


(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) 
                           env))
      (eval# (if-consequent exp) env)
      (eval# (if-alternative exp) env)))


(define (install-syntax)
  (put-syntax! 'quote eval-quoted) 
  (put-syntax! 'define eval-definition) 
  (put-syntax! 'set! eval-assignment) 
  (put-syntax! 'if eval-if) 
  (put-syntax! 'lambda eval-lambda) 
  (put-syntax! 'begin eval-begin) 
  (put-syntax! 'cond eval-cond) 
  'done)

(install-syntax)

(define input-prompt  ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value 
                   input 
                   the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


(define the-global-environment 
  (setup-environment))

; (driver-loop)


; (interpret
; '(begin
; (define count 0)
; (define (id x) (set! count (+ count 1)) x)
; (define w (id (id 10)))
; count
; )
; )
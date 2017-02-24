;Exercise 5.18: Modify the make-register procedure of 5.2.1 so that registers can be traced. Registers should accept messages that turn tracing on and off. When a register is traced, assigning a value to the register should print the name of the register, the old contents of the register, and the new contents being assigned. Extend the interface to the machine model to permit you to turn tracing on and off for designated machine registers.
(load "/Users/soulomoon/git/SICP/Chapter5/ch5-regsim.scm")
; register have a inner state trace, turn it on by setting it to be trace-on and off trace-off
; add a new dispatch to the make-new-machine, dispatch 'trace-on with lambda that accept register's name to turn it on
; trace is off by default
(define (make-register name)
  (let ((trace-on
            (lambda (name contents value)
                    (display "reg-name: ")
                    (display name)
                    (newline )
                    (display "old-content:")
                    (display contents)
                    (newline )
                    (display "new-content:")
                    (display value)
                    (newline )
                    (newline )
                    (newline )))
         (trace-off (lambda (name contents value) 'done)))
    (let ((trace trace-off))
      (let ((contents '*unassigned*))
        (define (dispatch message)
          (cond ((eq? message 'get) contents)
                ((eq? message 'trace-on) (set! trace trace-on))
                ((eq? message 'trace-off) (set! trace trace-off))
                ((eq? message 'set)
                 (lambda (value) (trace name contents value) (set! contents value)))
                (else
                 (error "Unknown request -- REGISTER" message))))
        dispatch))))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ;return a lambda that accept a register name and set trace-on
              ((eq? message 'trace-on) (lambda (name) ((lookup-register name) 'trace-on)))
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))
(define (print x)
  (newline )
  (display x)
  (newline ))

;Recursive exponentiation:
(define fib-machine
(make-machine
  '(n val continue)
  (list (list '< <) (list '- -) (list 'print print) (list '+ +))
  '(
     (assign continue (label fib-done))
    fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n − 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)           ; save old value of n
     (assign n
             (op -)
             (reg n)
             (const 1)) ; clobber n to n-1
    ; (perform (op trace-off))
     (goto
      (label fib-loop)) ; perform recursive call
    afterfib-n-1 ; upon return, val contains Fib(n − 1)
     (restore n)
    ; (restore continue) ;here extra-restore
     ;; set up to compute Fib(n − 2)
     (assign n (op -) (reg n) (const 2))
    ; (save continue) ;here extra-save
     (assign continue (label afterfib-n-2))
     (save val)         ; save Fib(n − 1)
     (goto (label fib-loop))
    afterfib-n-2 ; upon return, val contains Fib(n − 2)
     (assign n
             (reg val)) ; n now contains Fib(n − 2)
     (restore val)      ; val now contains Fib(n − 1)
     (restore continue)
     (assign val        ; Fib(n − 1) + Fib(n − 2)
             (op +)
             (reg val)
             (reg n))
     (goto              ; return to caller,
      (reg continue))   ; answer is in val
    immediate-answer
     (assign val
             (reg n))   ; base case: Fib(n) = n
     (goto (reg continue))
    fib-done
    (perform (op print) (reg val))
    ;(perform (op instruction-counter))
)))
((fib-machine 'trace-on) 'n)
(set-register-contents! fib-machine 'n 3)
(start fib-machine)


;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;(REGISTER SIMULATOR LOADED)
;reg-name: n
;old-content:*unassigned*
;new-content:3
;
;
;'done
;reg-name: n
;old-content:3
;new-content:2
;
;
;reg-name: n
;old-content:2
;new-content:1
;
;
;reg-name: n
;old-content:1
;new-content:2
;
;
;reg-name: n
;old-content:2
;new-content:0
;
;
;reg-name: n
;old-content:0
;new-content:0
;
;
;reg-name: n
;old-content:0
;new-content:3
;
;
;reg-name: n
;old-content:3
;new-content:1
;
;
;reg-name: n
;old-content:1
;new-content:1
;
;
;
;2
;'done
;>

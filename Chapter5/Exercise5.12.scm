;Exercise 5.12: The simulator can be used to help determine the data paths required for implementing a machine with a given controller. Extend the assembler to store the following information in the machine model:
;
;a list of all instructions, with duplicates removed, sorted by instruction type (assign, goto, and so on);
;a list (without duplicates) of the registers used to hold entry points (these are the registers referenced by goto instructions);
;a list (without duplicates) of the registers that are saved or restored;
;for each register, a list (without duplicates) of the sources from which it is assigned (for example, the sources for register val in the factorial machine of Figure 5.11 are (const 1) and ((op *) (reg n) (reg val))).
;Extend the message-passing interface to the machine to provide access to this new information. To test your analyzer, define the Fibonacci machine from Figure 5.12 and examine the lists you constructed.
(load "/Users/soulomoon/git/SICP/Chapter5/ch5-regsim.scm")
(define (print x)
  (newline )
  (display x)
  (newline ))
(define (filter f isequence)
  (cond ((null? isequence)
          isequence)
        ((f (car isequence))
          (cons (car isequence) (filter f (cdr isequence))))

        (else (filter f (cdr isequence)))))
(define (symbol>? a b)
  (string>? (symbol->string a) (symbol->string b)))
(define (sort isequence)
  (define (loop sq)
    (cond
      ((or (null? sq) (null? (cdr sq)))
        sq)
      ((symbol>? (caar sq) (caar (cdr sq)))
        (cons (cadr sq) (loop (cons (car sq) (cddr sq)))))
      (else
        (cons (car sq) (loop (cdr sq))))))
  (let ((next (loop isequence)))
    (if (equal? isequence next)
        isequence
        (sort next))))
(define (member? s sq)
  (cond ((null? sq)
          false)
        ((equal? s (car sq))
          true)
        (else (member? s (cdr sq)))))
(define (unique isequence)
    (cond
        ((null? isequence)
          isequence)
        ((member? (car isequence) (cdr isequence))
          (unique (cdr isequence)))
        (else
          (cons (car isequence) (unique (cdr isequence))))))

(define (goto-register? inst)
  (if (and (tagged-list? inst 'goto) (register-exp? (goto-dest inst)))
      true
      false))

(define (save-restore? inst)
  (if (or (tagged-list? inst 'save) (tagged-list? inst 'restore))
      true
      false))

(define (make-assign? name)
  (define (assign? inst)
    (if (and (tagged-list? inst 'assign) (eq? (assign-reg-name inst) name))
        true
        false))
  assign?)

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-isequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))

      (define (get-instructions)
        (sort (unique (map car the-instruction-isequence))))

      (define (get-goto-register)
        (map (lambda (inst) (register-exp-reg (goto-dest inst)))
             (filter goto-register? (get-instructions))))

      (define (save-restore-register)
        (unique
          (map (lambda (inst) (stack-inst-reg-name inst))
             (filter save-restore? (get-instructions)))))

      (define (assign-source)
        (map
          (lambda (name)
            (cons name
              (unique
                (map (lambda (inst) (assign-value-exp inst))
                   (filter (make-assign? name) (get-instructions))))))
          (map car register-table)
        ))


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
               (set-contents! pc the-instruction-isequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-isequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'get-instructions) (get-instructions))
              ((eq? message 'get-goto-register) (get-goto-register))
              ((eq? message 'save-restore-register) (save-restore-register))
              ((eq? message 'assign-source) (assign-source))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

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
)))
(define (show-list l)
  (for-each (lambda (x) (display x)(newline )) l)(newline))

(show-list (fib-machine 'get-instructions))
(show-list (fib-machine 'get-goto-register))
(show-list (fib-machine 'save-restore-register))
(show-list (fib-machine 'assign-source))
;
;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;(REGISTER SIMULATOR LOADED)
;(assign continue (label fib-done))
;(assign continue (label afterfib-n-1))
;(assign n (op -) (reg n) (const 1))
;(assign n (op -) (reg n) (const 2))
;(assign continue (label afterfib-n-2))
;(assign n (reg val))
;(assign val (op +) (reg val) (reg n))
;(assign val (reg n))
;(branch (label immediate-answer))
;(goto (label fib-loop))
;(goto (reg continue))
;(perform (op print) (reg val))
;(restore n)
;(restore val)
;(restore continue)
;(save continue)
;(save n)
;(save val)
;(test (op <) (reg n) (const 2))
;
;continue
;
;continue
;n
;val
;
;(continue ((label fib-done)) ((label afterfib-n-1)) ((label afterfib-n-2)))
;(val ((op +) (reg val) (reg n)) ((reg n)))
;(n ((op -) (reg n) (const 1)) ((op -) (reg n) (const 2)) ((reg val)))
;(pc)
;(flag)
;
;>

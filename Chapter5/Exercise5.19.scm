;Exercise 5.19: Alyssa P. Hacker wants a breakpoint feature in the simulator to help her debug her machine designs. You have been hired to install this feature for her. She wants to be able to specify a place in the controller sequence where the simulator will stop and allow her to examine the state of the machine. You are to implement a procedure
;
;(set-breakpoint ⟨machine⟩ ⟨label⟩ ⟨n⟩)
;that sets a breakpoint just before the nthnth instruction after the given label. For example,
;
;(set-breakpoint gcd-machine 'test-b 4)
;installs a breakpoint in gcd-machine just before the assignment to register a. When the simulator reaches the breakpoint it should print the label and the offset of the breakpoint and stop executing instructions. Alyssa can then use get-register-contents and set-register-contents! to manipulate the state of the simulated machine. She should then be able to continue execution by saying
;
;(proceed-machine ⟨machine⟩)
;She should also be able to remove a specific breakpoint by means of
;
;(cancel-breakpoint ⟨machine⟩ ⟨label⟩ ⟨n⟩)
;or to remove all breakpoints by means of
;
;(cancel-all-breakpoints ⟨machine⟩)

;Exercise 5.17: Extend the instruction tracing of Exercise 5.16 so that before printing an instruction, the simulator prints any labels that immediately precede that instruction in the controller sequence. Be careful to do this in a way that does not interfere with instruction counting (Exercise 5.15). You will have to make the simulator retain the necessary label information.
(load "/Users/soulomoon/git/SICP/Chapter5/ch5-regsim.scm")
;use 5.17 to gain the ability count up label if the inst following the label is
;to be run

;make a new data STRUCTURE named label-counter, and a bunch of helper function
;around it to assist
;(label counter number . breakpoints)
; get help from list operation of remove


(define (make-label-counter label n)
  (list label 0 n))

(define (get-label-count lb)
  (cadr lb))
(define (get-label-breakpoints lb)
  (cddr lb))

(define (add-label-breakpoint lb n)
  (if (not (member n (get-label-breakpoints lb)))
      (set-cdr! (cdr lb) (cons n (get-label-breakpoints lb)))))
(define (delete-label-breakpoint lb n)
  (if (member n (get-label-breakpoints lb))
      (set-cdr! (cdr lb) (remove n (get-label-breakpoints lb)))))
(define (breakpoint-label-count? lb)
  (let ((result (member (cadr lb) (get-label-breakpoints lb))))
        (if result
            (delete-label-breakpoint lb (cadr lb)))
        result))
(define (plus-label-counter lb)
  (set-car! (cdr lb) (+ 1 (get-label-count lb))))

(define (remove n l)
  (cond ((null? l) nil)
        ((= n (car l)) (cdr l))
        (else (cons (car l) (remove n (cdr l))))))


(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)
      nil ))

(define (extract-labels text receive last-label)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels))
               (receive (cons (make-instruction next-inst last-label)
                              insts)
                        labels))))
        (if (symbol? (car text))
            (car text)
            nil))))
(define (set-instruction-execution-proc! inst proc)
  (set-car! (cdr inst) proc))

(define (make-instruction text last-label)
  (list text '() last-label))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cadr inst))

(define (instruction-last-label inst)
  (caddr inst))



(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (label-counters '())
        (trace-on
          (lambda (inst)
                  (display (instruction-last-label inst))
                  (display ":")
                  (display (instruction-text inst))
                  (newline )))
       (trace-off (lambda (inst) 'done))
        (instruction-counter 0))
    (let ((trace trace-off))
      (let ((the-ops
             (list
                   (list 'trace-off
                         (lambda () (set! trace trace-off)))
                   (list 'trace-on
                         (lambda () (set! trace trace-on)))
                   (list 'initialize-stack
                         (lambda () (stack 'initialize)))
                   ;;**next for monitored stack (as in section 5.2.4)
                   ;;  -- comment out if not wanted
                   (list 'print-stack-statistics
                         (lambda () (stack 'print-statistics)))
                   (list 'instruction-counter
                         (lambda () (display "instruction-counter: ")
                                    (display instruction-counter)
                                    (newline )
                                    (set! instruction-counter 0)))))
            (register-table
             (list (list 'pc pc) (list 'flag flag))))
      (define (get-label-counter label)
        (assoc label label-counters))

      (define (set-breakpoint label n)
        (if (get-label-counter label)
            (add-label-breakpoint (get-label-counter label) n)
            (set! label-counters (cons (make-label-counter label n) label-counters))))

      (define (cancel-breakpoint label n)
        (let ((label-counter (get-label-counter label)))
              (if label-counter
                  (delete-label-breakpoint label-counter n))))
      (define (cancel-all-breakpoints)
              (set! label-counters '()))

      (define (counter-addone label)
        (let ((label-counter (get-label-counter label)))
              (if label-counter
                  (plus-label-counter label-counter))))
      (define (reach-breakpoint? label)
        (let ((label-counter (get-label-counter label)))
          (if label-counter
              (breakpoint-label-count? label-counter)
              false)))

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
                (counter-addone (instruction-last-label (car insts)))
                (if (not (reach-breakpoint? (instruction-last-label (car insts))))
                    (begin
                      ((instruction-execution-proc (car insts)))
                      (execute)))))))
                ;((instruction-execution-proc (car insts)))
                ;(execute)))))

      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))

              ((eq? message 'proceed-machine)
                (execute))
              ((eq? message 'cancel-all-breakpoints)
                (cancel-all-breakpoints))
              ((eq? message 'set-breakpoint)
                (lambda (label n)
                  (set-breakpoint label n)))
              ((eq? message 'cancel-breakpoint)
                (lambda (label n)
                  (cancel-breakpoint label n)))

              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'trace-on) (set! trace trace-on))
              ((eq? message 'trace-off) (set! trace trace-off))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch))))
(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))
(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))
(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))
(define (proceed-machine machine)
  (machine 'proceed-machine))

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
     (perform (op trace-off))
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
(set-register-contents! fib-machine 'n 8)

(set-breakpoint fib-machine 'fib-loop 2)
(set-breakpoint fib-machine 'fib-loop 3)
(cancel-all-breakpoints fib-machine)
(start fib-machine)
(print "one")

(set-breakpoint fib-machine 'fib-loop 2)
(set-breakpoint fib-machine 'fib-loop 3)
(start fib-machine)
(print "two")
(cancel-breakpoint fib-machine 'fib-loop 3)
(proceed-machine fib-machine)

;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;(REGISTER SIMULATOR LOADED)
;'done
;
;21
;'done
;
;one
;
;two
;
;21
;'done
;>

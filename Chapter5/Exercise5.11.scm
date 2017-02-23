;Exercise 5.11: When we introduced save and restore in 5.1.4, we didn’t specify what would happen if you tried to restore a register that was not the last one saved, as in the sequence
;
;(save y)
;(save x)
;(restore y)
;There are several reasonable possibilities for the meaning of restore:
;
;(restore y) puts into y the last value saved on the stack, regardless of what register that value came from. This is the way our simulator behaves. Show how to take advantage of this behavior to eliminate one instruction from the Fibonacci machine of 5.1.4 (Figure 5.12).
;(restore y) puts into y the last value saved on the stack, but only if that value was saved from y; otherwise, it signals an error. Modify the simulator to behave this way. You will have to change save to put the register name on the stack along with the value.
;(restore y) puts into y the last value saved from y regardless of what other registers were saved after y and not restored. Modify the simulator to behave this way. You will have to associate a separate stack with each register. You should make the initialize-stack operation initialize all the register stacks.
(load "/Users/soulomoon/git/SICP/Chapter5/ch5-regsim.scm")
(define (print x)
  (newline )
  (display x)
  (newline ))
;1
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

;2 when pop and push, in save and restore, we pass in the reg name with the content
(define (make-save2 inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (cons (stack-inst-reg-name inst) (get-contents reg)))
      (advance-pc pc))))

(define (make-restore2 inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (let ((poped-item (pop stack)))
        (if (eq? (car poped-item) (stack-inst-reg-name inst))
            (set-contents! reg (cdr poped-item))
            (error "make-restore : not from the same reg: " (car poped-item) "" (stack-inst-reg-name inst)))
            (advance-pc pc)))))

;3 add a new stack for every
(define (make-save3 inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      ((stack 'switch) (stack-inst-reg-name inst))
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore3 inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      ((stack 'switch) (stack-inst-reg-name inst))
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (make-stack3)
  (let ((s '())
        ;so I could set-cdr!
        (current-stack (cons '() '()))
        (stack-list '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      (set! current-stack (cons '() '()))
      (set! stack-list '())
      'done)

    (define (switch name)
      (define (new name)
        (set! stack-list (cons (cons name '()) stack-list)))
      (if (assoc name stack-list)
          (begin
            ;save the current-stack
            (set-cdr! current-stack s)
            (set! current-stack (assoc name stack-list))
            (set! s (cdr current-stack)))
          (begin

            (new name)
            (switch name))))

    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'switch) switch)
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))


; for 3
;(define make-save make-save3)
;(define make-restore make-restore3)
;(define make-stack make-stack3)

;for 2
;(define make-save make-save2)
;(define make-restore make-restore2)
(define go-machine
  (make-machine
    '(a b)
    (list (list 'print print))
    '(
      (assign a (const 1))
      (assign b (const 2))
      (save a)
      (restore a)
      (perform (op print) (reg a))
      (perform (op print) (reg b))
    )))
(start go-machine)
(define go-machine
  (make-machine
    '(a b)
    (list (list 'print print))
    '(
      (assign a (const 1))
      (assign b (const 2))
      (save a)
      (save b)
      (restore a)
      (restore b)
      (perform (op print) (reg a))
      (perform (op print) (reg b))
    )))
  (start go-machine)

(define go-machine
  (make-machine
    '(a b)
    (list (list 'print print))
    '(
      (assign a (const 1))
      (assign b (const 2))
      (save a)
      (restore b)
      (perform (op print) (reg a))
      (perform (op print) (reg b))
    )))
  (start go-machine)

  ;Welcome to DrRacket, version 6.8 [3m].
  ;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
  ;(REGISTER SIMULATOR LOADED)
  ;
  ;1
  ;
  ;2
  ;'done
  ;
  ;1
  ;
  ;2
  ;'done
  ;. . Empty stack -- POP
  ;>

  ;Welcome to DrRacket, version 6.8 [3m].
  ;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
  ;(REGISTER SIMULATOR LOADED)
  ;
  ;1
  ;
  ;2
  ;'done
  ;. . make-restore : not from the same reg:  b "" a
  ;>

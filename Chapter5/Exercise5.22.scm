;Exercise 5.22: Exercise 3.12 of 3.3.1 presented an append procedure that appends two lists to form a new list and an append! procedure that splices two lists together. Design a register machine to implement each of these procedures. Assume that the list-structure memory operations are available as primitive operations.
(load "/Users/soulomoon/git/SICP/Chapter5/ch5-regsim.scm")
(define append-machine
(make-machine
  '(x y temp continue)
  (list
    (list 'cdr cdr)
    (list 'car car)
    (list 'cons cons)
    (list 'print print)
    (list 'null? null?))
  '(
    (assign continue (label append-done))
  append-loop
    ;save continue for later use
    (save continue)
    (test (op null?) (reg x))
    (branch (label append-null))

    ;assign continue
    (assign continue (label append-after))

    ;split x
    (assign temp (op cdr) (reg x))
    (assign x (op car) (reg x))
    (save x)
    (assign x (reg temp))
    (goto (label append-loop))
  ;base
  append-null
    (restore continue)
    (goto (reg continue))

  append-after
    ;get the stored car x
    (restore x)
    (assign y (op cons) (reg x) (reg y))
    (restore continue)
    (goto (reg continue))

  append-done
    (perform (op print) (reg y))
)))

(define append!-machine
(make-machine
  '(x y tempx cdrx)
  (list
    (list 'cdr cdr)
    (list 'set-cdr! set-cdr!)
    (list 'car car)
    (list 'cons cons)
    (list 'print print)
    (list 'null? null?))
  '(
    (assign tempx (reg x))
  last-pair-loop
    (assign cdrx (op cdr) (reg tempx))
    (test (op null?) (reg cdrx))
    (branch (label append-begin))
    (assign tempx (reg cdrx))
    (goto (label last-pair-loop))
  append-begin
    (perform (op set-cdr!) (reg tempx) (reg y))
  append-done
    (perform (op print) (reg x))
)))

(define x '(1 2))
(define y '(3 4))
(set-register-contents! append-machine 'x x)
(set-register-contents! append-machine 'y y)
(start append-machine)
(set-register-contents! append!-machine 'x x)
(set-register-contents! append!-machine 'y y)
(start append!-machine)

;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;(REGISTER SIMULATOR LOADED)
;'done
;'done
;
;(1 2 3 4)
;'done
;'done
;'done
;
;(1 2 3 4)
;'done
;>

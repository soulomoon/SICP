;Exercise 5.14: Measure the number of pushes and the maximum stack depth required to compute n!n! for various small values of nn using the factorial machine shown in Figure 5.11. From your data determine formulas in terms of nn for the total number of push operations and the maximum stack depth used in computing n!n! for any n>1n>1. Note that each of these is a linear function of nn and is thus determined by two constants. In order to get the statistics printed, you will have to augment the factorial machine with instructions to initialize the stack and print the statistics. You may want to also modify the machine so that it repeatedly reads a value for nn, computes the factorial, and prints the result (as we did for the GCD machine in Figure 5.4), so that you will not have to repeatedly invoke get-register-contents, set-register-contents!, and start.
(load "/Users/soulomoon/git/SICP/Chapter5/ch5-regsim.scm")

(define (print x)
  (newline )
  (display x)
  (newline ))

;Recursive exponentiation:
(define fib-machine
(make-machine
  '(n val continue next)
  (list (list '< <) (list '- -) (list 'print print) (list '+ +) (list '= =) (list '* *))
  '(
;compute over
 again
   (perform (op initialize-stack))
   (assign next (op -) (reg next) (const 1))
   (test (op <) (reg next) (const 1))
   (branch (label ending))
   (assign n (reg next))
;old one
   (assign continue (label fact-done))   ; set up final return address
 fact-loop
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   ;; Set up for the recursive call by saving n and continue.
   ;; Set up continue so that the computation will continue
   ;; at after-fact when the subroutine returns.
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-fact))
   (goto (label fact-loop))
 after-fact
   (restore n)
   (restore continue)
   (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
   (goto (reg continue))                   ; return to caller
 base-case
   (assign val (const 1))                  ; base case: 1! = 1
   (goto (reg continue))                   ; return to caller
 fact-done
   (perform (op print-stack-statistics))
   (perform (op print) (reg next))
  ; (perform (op print) (reg val))
   (goto (label again))
 ending
 )
 ))

(set-register-contents! fib-machine 'next 10)
(start fib-machine)

;total-pushes = total-pushes
;total-pushes = 2 * n - 2


;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;(REGISTER SIMULATOR LOADED)
;'done
;
;(total-pushes = 16 total-pushes = 16)
;9
;
;(total-pushes = 14 maximum-depth = 14)
;8
;
;(total-pushes = 12 maximum-depth = 12)
;7
;
;(total-pushes = 10 maximum-depth = 10)
;6
;
;(total-pushes = 8 maximum-depth = 8)
;5
;
;(total-pushes = 6 maximum-depth = 6)
;4
;
;(total-pushes = 4 maximum-depth = 4)
;3
;
;(total-pushes = 2 maximum-depth = 2)
;2
;
;(total-pushes = 0 maximum-depth = 0)
;1
;'done
;>

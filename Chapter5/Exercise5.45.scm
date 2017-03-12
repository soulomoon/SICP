;Exercise 5.45: By comparing the stack operations used by compiled code to the stack operations used by the evaluator for the same computation, we can determine the extent to which the compiler optimizes use of the stack, both in speed (reducing the total number of stack operations) and in space (reducing the maximum stack depth). Comparing this optimized stack use to the performance of a special-purpose machine for the same computation gives some indication of the quality of the compiler.
;
;Exercise 5.27 asked you to determine, as a function of nn, the number of pushes and the maximum stack depth needed by the evaluator to compute n!n! using the recursive factorial procedure given above. Exercise 5.14 asked you to do the same measurements for the special-purpose factorial machine shown in Figure 5.11. Now perform the same analysis using the compiled factorial procedure.
;Take the ratio of the number of pushes in the compiled version to the number of pushes in the interpreted version, and do the same for the maximum stack depth. Since the number of operations and the stack depth used to compute n!n! are linear in nn, these ratios should approach constants as nn becomes large. What are these constants? Similarly, find the ratios of the stack usage in the special-purpose machine to the usage in the interpreted version.
;
;Compare the ratios for special-purpose versus interpreted code to the ratios for compiled versus interpreted code. You should find that the special-purpose machine does much better than the compiled code, since the hand-tailored controller code should be much better than what is produced by our rudimentary general-purpose compiler.
;
;Can you suggest improvements to the compiler that would help it generate code that would come closer in performance to the hand-tailored version?

;;;;;;;load my own entry takes two parameters one for compile and go and one for interprete
(load "/Users/soulomoon/git/SICP/Chapter5/compile-and-interpret.scm")

;;;;;;1 analysis of stack uses on factorial between interpreter and compile-and run
(go-two
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n)))
 '(
   (factorial 6)
   (factorial 7)
   (factorial 8)
   (factorial 9)
   (factorial 10)
   (define (factorial2 n)
      (if (= n 1)
          1
          (* (factorial (- n 1)) n)))
   (factorial2 6)
   (factorial2 7)
   (factorial2 8)
   (factorial2 9)
   (factorial2 10)
   ))
;compiled factorial
;total-pushes: 6n + 1
;maximum-depth: 3n - 1

;interpret factorial
;total-pushes: 6n + 27
;maximum-depth: 3n + 1

;special-purpose factorial
;total-pushes: 2n - 2
;maximum-depth: 2n - 2


;;; 2 compile it with the open-coded compiler
;have more register so you don't have to stored so many times.

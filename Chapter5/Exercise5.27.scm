;Exercise 5.27: For comparison with Exercise 5.26, explore the behavior of the following procedure for computing factorials recursively:
;
;(define (factorial n)
;  (if (= n 1)
;      1
;      (* (factorial (- n 1)) n)))
;By running this procedure with the monitored stack, determine, as a function of nn, the maximum depth of the stack and the total number of pushes used in evaluating n!n! for n≥1n≥1. (Again, these functions will be linear.) Summarize your experiments by filling in the following table with the appropriate expressions in terms of nn:
;RecursivefactorialIterativefactorialMaximumdepthNumber ofpushes
;MaximumNumber ofdepthpushesRecursivefactorialIterativefactorial
;The maximum depth is a measure of the amount of space used by the evaluator in carrying out the computation, and the number of pushes correlates well with the time required.
(load "/Users/soulomoon/git/SICP/material/allcode/load-eceval.scm")
(i
'(
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))


  (factorial 5)

)
)
            (maximum depth) (number of pushes)
recursive
factorial        3+5n           32n - 16

iterative
factorial         10             35n + 29

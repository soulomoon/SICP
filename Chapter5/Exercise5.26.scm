  ;Exercise 5.26: Use the monitored stack to explore the tail-recursive property of the evaluator (5.4.2). Start the evaluator and define the iterative factorial procedure from 1.2.1:
  ;
  ;(define (factorial n)
  ;  (define (iter product counter)
  ;    (if (> counter n)
  ;        product
  ;        (iter (* counter product)
  ;              (+ counter 1))))
  ;  (iter 1 1))
  ;Run the procedure with some small values of nn. Record the maximum stack depth and the number of pushes required to compute n!n! for each of these values.
  ;
  ;You will find that the maximum depth required to evaluate n!n! is independent of nn. What is that depth?
  ;Determine from your data a formula in terms of nn for the total number of push operations used in evaluating n!n! for any n≥1n≥1. Note that the number of operations used is a linear function of nn and is thus determined by two constants.
(load "/Users/soulomoon/git/SICP/material/allcode/load-eceval.scm")
; the total depth would be 10
; 35n + 29
  (i
  '(
  (define (factorial n)
    (define (iter product counter)
      (if (> counter n)
          product
          (iter (* counter product)
                (+ counter 1))))
    (iter 1 1))

  (factorial 1)
  (factorial 2)
  (factorial 3)
  (factorial 4)
  (factorial 5)
  ))

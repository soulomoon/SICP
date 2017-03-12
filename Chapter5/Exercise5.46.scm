;Exercise 5.46: Carry out an analysis like the one in Exercise 5.45 to determine the effectiveness of compiling the tree-recursive Fibonacci procedure
;
;(define (fib n)
;  (if (< n 2)
;      n
;      (+ (fib (- n 1)) (fib (- n 2)))))
;compared to the effectiveness of using the special-purpose Fibonacci machine of Figure 5.12. (For measurement of the interpreted performance, see Exercise 5.29.) For Fibonacci, the time resource used is not linear in n;n; hence the ratios of stack operations will not approach a limiting value that is independent of nn.
(load "/Users/soulomoon/git/SICP/Chapter5/compile-and-interpret.scm")

(go-two
 '(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

 '(
   (fib 6)
   (fib 7)
   (fib 8)
   (fib 9)
   (fib 10)
   (define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2)))))

   (fib 6)
   (fib 7)
   (fib 8)
   (fib 9)
   (fib 10)
   ))
; the depth is still linear to n: an + b
;interpret: a=5
;compiled: a=3
; still we get a⋅Fib(n+1)+b for all pushes

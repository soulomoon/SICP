;Exercise 5.29: Monitor the stack operations in the tree-recursive Fibonacci computation:
;
;(define (fib n)
;  (if (< n 2)
;      n
;      (+ (fib (- n 1)) (fib (- n 2)))))
;Give a formula in terms of nn for the maximum depth of the stack required to compute Fib(n)Fib(n) for n≥2n≥2. Hint: In 1.2.2 we argued that the space used by this process grows linearly with nn.
;Give a formula for the total number of pushes used to compute Fib(n)Fib(n) for n≥2n≥2. You should find that the number of pushes (which correlates well with the time used) grows exponentially with nn. Hint: Let S(n)S(n) be the number of pushes used in computing Fib(n)Fib(n). You should be able to argue that there is a formula that expresses S(n)S(n) in terms of S(n−1)S(n−1), S(n−2)S(n−2), and some fixed “overhead” constant kk that is independent of nn. Give the formula, and say what kk is. Then show that S(n)S(n) can be expressed as a⋅Fib(n+1)+ba⋅Fib(n+1)+b and give the values of aa and bb.
(load "/Users/soulomoon/git/SICP/material/allcode/load-eceval.scm")
;;;1
;maximum-depth: 5n-2
;;;2
;S(n−1)+S(n−2)+40=S(n); k = 40
;a=56
;b=-40


(i
'(
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
))

; Exercise 1.19. There is a clever algorithm for computing the Fibonacci numbers in a logarithmic number of steps. Recall the transformation of the state variables a and b in the fib-iter process of section 1.2.2:
; ab←a+b←a
; a←a+bb←a
; Call this transformation T, and observe that applying T over and over again n times, starting with 1 and 0, produces the pair Fib(n + 1) and Fib(n). In other words, the Fibonacci numbers are produced by applying Tn, the nth power of the transformation T, starting with the pair (1,0). Now consider T to be the special case of p = 0 and q = 1 in a family of transformations TpqTpq, where TpqTpq transforms the pair (a,b)(a,b) according to
; ab←bq+aq+ap←bp+aq
; a←bq+aq+apb←bp+aq
; Show that if we apply such a transformation TpqTpq twice, the effect is the same as using a single transformation Tp′q′Tp′q′ of the same form, and compute p′p′ and q′q′ in terms of pp and qq. This gives us an explicit way to square these transformations, and thus we can compute TnTn using successive squaring, as in the fast-expt procedure. Put this all together to complete the following procedure, which runs in a logarithmic number of steps:41
; #lang planet neil/sicp

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ p q)      ; compute p'
                   (+ p (+ q q))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib 111)
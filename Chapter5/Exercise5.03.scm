; Exercise 5.2: Use the register-machine language to describe the iterative factorial machine of Exercise 5.1.
(load "/Users/soulomoon/git/SICP/Chapter5/ch5-regsim.scm")

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))
  
(define GCD-machine
(make-machine
  '(g x)
  (list (list 'good-enough? good-enough?) (list 'improve improve))
  '(
    (assign g (const 1.0))
  test->
    (test (op good-enough?) (reg g) (reg x))
    (branch (label GCD-done))
    (assign g (op improve) (reg g) (reg x))
    (goto (label test->))
  GCD-done
)))


(set-register-contents! GCD-machine 'x 2)
(start GCD-machine)
(get-register-contents GCD-machine 'g)

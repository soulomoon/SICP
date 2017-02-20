;Exercise 5.3: Design a machine to compute square roots using Newton’s method, as described in 1.1.7:
;
;(define (sqrt x)
;  (define (good-enough? guess)
;    (< (abs (- (square guess) x)) 0.001))
;  (define (improve guess)
;    (average guess (/ x guess)))
;  (define (sqrt-iter guess)
;    (if (good-enough? guess)
;        guess
;        (sqrt-iter (improve guess))))
;  (sqrt-iter 1.0))
;Begin by assuming that good-enough? and improve operations are available as primitives. Then show how to expand these in terms of arithmetic operations. Describe each version of the sqrt machine design by drawing a data-path diagram and writing a controller definition in the register-machine language.(load "/Users/soulomoon/git/SICP/Chapter5/ch5-regsim.scm")
(load "/Users/soulomoon/git/SICP/Chapter5/ch5-regsim.scm")

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))
(define (print x)
  (newline )
  (display x)
  (newline ))
(define GCD-machine
(make-machine
  '(g x)
  (list (list 'good-enough? good-enough?) (list 'improve improve) (list 'print print))
  '(
    (assign g (const 1.0))
  test->
    (test (op good-enough?) (reg g) (reg x))
    (branch (label GCD-done))
    (assign g (op improve) (reg g) (reg x))
    (goto (label test->))
  GCD-done
    (perform (op print) (reg g))
)))

(define GCD-machine2
(make-machine
  '(g x t)
  (list
    (list '< <)
    (list '- -)
    (list 'abs abs)
    (list 'square square)
    (list 'improve improve)
    (list 'print print))
  '(
    (assign g (const 1.0))
  test->
    (assign t (op square) (reg g))
    (assign t (op -) (reg t) (reg x))
    (assign t (op abs) (reg t))
    (test (op <) (reg t) (const 0.001))
    (branch (label GCD-done))
    (assign g (op improve) (reg g) (reg x))
    (goto (label test->))
  GCD-done
    (perform (op print) (reg g))
)))

(define GCD-machine3
(make-machine
  '(g x t)
  (list
    (list '< <)
    (list '- -)
    (list '/ /)
    (list 'abs abs)
    (list 'square square)
    (list 'average average)
    (list 'improve improve)
    (list 'print print))
  '(
    (assign g (const 1.0))
  test->
    (assign t (op square) (reg g))
    (assign t (op -) (reg t) (reg x))
    (assign t (op abs) (reg t))
    (test (op <) (reg t) (const 0.001))
    (branch (label GCD-done))
    (assign t (op /) (reg x) (reg g))
    (assign g (op average) (reg g) (reg t))
    (goto (label test->))
  GCD-done
    (perform (op print) (reg g))
)))


(set-register-contents! GCD-machine 'x 2)
(start GCD-machine)
(set-register-contents! GCD-machine2 'x 2)
(start GCD-machine2)
(set-register-contents! GCD-machine3 'x 2)
(start GCD-machine3)

;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;(REGISTER SIMULATOR LOADED)
;'done
;
;1.4142156862745097
;'done
;'done
;
;1.4142156862745097
;'done
;'done
;
;1.4142156862745097
;'done
;>

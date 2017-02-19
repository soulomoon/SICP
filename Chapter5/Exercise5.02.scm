; Exercise 5.2: Use the register-machine language to describe the iterative factorial machine of Exercise 5.1.
(load "/Users/soulomoon/git/SICP/Chapter5/ch5-regsim.scm")
(define factorial-machine
(make-machine
  '(c p n)
  (list (list '+ +) (list '* *) (list '> >))
  '(
    (assign c (const 1))
    (assign p (const 1))
  test->
    (test (op >) (reg c) (reg n))
    (branch (label factorial-done))
    (assign p (op *) (reg c) (reg p))
    (assign c (op +) (reg c) (const 1))
    (goto (label test->))
  factorial-done
)))

(define factorial-machine1
(make-machine
  '(c p r t n)
  (list (list '+ +) (list '* *) (list '> >))
  '(
    (assign c (const 1))
    (assign p (const 1))
  test-c
    (test (op >) (reg c) (reg n))
    (branch (label f-done))
    (assign r (op *) (reg c) (reg p))
    (assign p r)
    (assign t (op +) (reg c) (const 1))
    (assign c t)
    (goto (label test-c))
  f-done
)))

(set-register-contents! factorial-machine 'n 5)
(start factorial-machine)
(get-register-contents factorial-machine 'p)

(set-register-contents! factorial-machine1 'n 5)
(start factorial-machine1)
(get-register-contents factorial-machine1 'p)

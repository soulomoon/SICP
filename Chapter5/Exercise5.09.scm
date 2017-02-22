;Exercise 5.9: The treatment of machine operations above permits them to operate on labels as well as on constants and the contents of registers. Modify the expression-processing procedures to enforce the condition that operations can be used only with registers and constants.
(load "/Users/soulomoon/git/SICP/Chapter5/ch5-regsim.scm")
(define (print x)
  (newline )
  (display x)
  (newline ))

(define go-machine
(make-machine
  '(a)
  (list (list 'print print))
  '(
   here
    (assign a (const 1))
    (perform (op print) (reg a))
    (perform (op print) (label here))
  )))
(start go-machine)

(define (make-operation-exp
         exp machine labels operations)
  (let ((op (lookup-prim
             (operation-exp-op exp)
             operations))
        (aprocs
         (map (lambda (e)
                (begin
                  (if (or (constant-exp? e) (register-exp? e))
                      (make-primitive-exp
                        e machine labels)
                      (error "make-operation-exp : not const or register :" "" e ""))))
              (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p))
                              aprocs)))))

(define go-machine
(make-machine
  '(a)
  (list (list 'print print))
  '(
   here
    (assign a (const 1))
    (perform (op print) (reg a))
    (perform (op print) (label here))
  )))
(start go-machine)

;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;(REGISTER SIMULATOR LOADED)
;
;1
;
;(((assign a (const 1)) . #<procedure:...5/ch5-regsim.scm:257:6>) ((perform (op print) (reg a)) . #<procedure:...5/ch5-regsim.scm:342:10>) ((perform (op print) (label here)) . #<procedure:...5/ch5-regsim.scm:342:10>))
;'done
;. . make-operation-exp : not const or register : "" (label here) ""
;>

;
;Exercise 5.41: Write a procedure find-variable that takes as arguments a variable and a compile-time environment and returns the lexical address of the variable with respect to that environment. For example, in the program fragment that is shown above, the compile-time environment during the compilation of expression ⟨e1⟩ is ((y z) (a b c d e) (x y)). Find-variable should produce
;
;(find-variable
; 'c '((y z) (a b c d e) (x y)))
;(1 2)
;
;(find-variable
; 'x '((y z) (a b c d e) (x y)))
;(2 0)
;
;(find-variable
; 'w '((y z) (a b c d e) (x y)))
(define (find-variable val env)
  (define (iter-var frame n)
    (cond
      ((null? frame) false)
      ((equal? val (car frame)) n)
      (else (iter-var (cdr frame) (+ n 1)))))
  (define (iter ev n)
    (if (null? ev)
        'not-found
        ;(error "find-variable not found! :" val)
        (let* ((current-frame (car ev))
              (tail (cdr ev))
              (var-pos (iter-var current-frame 0)))
              (if var-pos
                  (cons n var-pos)
                  (iter tail (+ n 1))))))
  (iter env 0))
;(find-variable
; 'c '((y z) (a b c d e) (x y)))
;;(1 2)
;
;(find-variable
; 'x '((y z) (a b c d e) (x y)))
;;(2 0)
;
;(find-variable
; 'w '((y z) (a b c d e) (x y)))
;not-found

;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;{mcons 1 2}
;{mcons 2 0}
;'not-found
;>

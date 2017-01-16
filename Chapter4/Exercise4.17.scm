; Exercise 4.17: Draw diagrams of the environment in effect when evaluating the expression ⟨e3⟩ in the procedure in the text, comparing how this will be structured when definitions are interpreted sequentially with how it will be structured if definitions are scanned out as described. Why is there an extra frame in the transformed program? Explain why this difference in environment structure can never make a difference in the behavior of a correct program. Design a way to make the interpreter implement the “simultaneous” scope rule for internal definitions without constructing the extra frame.


; 1 because transform would make an extra let, which make an extra frame

; 2 why it doesn't make a diffrence , because that frame is a protected frame, and the value would be overident in the next frame

; 3 
(load "/Users/soulomoon/git/SICP/Chapter4/Exercise4.06.scm")

(define (scan-out-defines body)
  (define (notdefinition? exp) (not (definition? exp)))
  (define (filter l predict?)
    (let ((returns '()))
         (define (iter l)
            (if (null? l)
                returns
                (if (predict? (car l))
                    (begin 
                      (set! returns (cons (car l) returns))                   (iter (cdr l)) )
                    (iter (cdr l)))))
          (iter l)))
  (define (defines) (filter body definition?))
  (define (notdefines) (filter body notdefinition?))
  (display (defines))(newline )
  
  (append (defines) (notdefines))
)

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

(interpret 
'(begin
  (define (test)
    (+ b (c))
  (define b 2)
  (define (c) 3)
  )
(test))
)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'ok
; ((define (c) 3) (define b 2))
; ()
; 5
; > 
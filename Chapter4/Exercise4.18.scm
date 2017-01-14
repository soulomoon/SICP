; Exercise 4.18: Consider an alternative strategy for scanning out definitions that translates the example in the text to

; (lambda ⟨vars⟩
;   (let ((u '*unassigned*)
;         (v '*unassigned*))
;     (let ((a ⟨e1⟩)
;           (b ⟨e2⟩))
;       (set! u a)
;       (set! v b))
;     ⟨e3⟩))
; Here a and b are meant to represent new variable names, created by the interpreter, that do not appear in the user’s program. Consider the solve procedure from 3.5.4:

; (define (solve f y0 dt)
;   (define y (integral (delay dy) y0 dt))
;   (define dy (stream-map f y))
;   y)
; Will this procedure work if internal definitions are scanned out as shown in this exercise? What if they are scanned out as shown in the text? Explain.

; 1
as text in this exercise, it would be an error because , when dy is definning , the y would be unassigned when assigning b.
; 2
whereas in the text it would be just fine because the y would be define when assigning dy the value of (stream-map f y)
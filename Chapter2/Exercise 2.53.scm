; Exercise 2.53: What would the interpreter print in response to evaluating each of the following expressions?

(display (list 'a 'b 'c))(newline)
; (a b c)
(display (list (list 'george)))(newline)
; ((george))
(display (cdr '((x1 x2) (y1 y2))))(newline)
; ((y1 y2)) 
(display (cadr '((x1 x2) (y1 y2))))(newline)
; (y1 y2)
(display (pair? (car '(a short list))))(newline)
; false
(display (memq 'red '((red shoes) (blue socks))))(newline)
; false
(display (memq 'red '(red shoes blue socks)))(newline)
; (red shoes blue socks)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; (a b c)
; ((george))
; ((y1 y2))
; (y1 y2)
; #f
; #f
; (red shoes blue socks)
; > 
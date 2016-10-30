; Exercise 1.26: Louis Reasoner is having great difficulty doing Exercise 1.24. His fast-prime? test seems to run more slowly than his prime? test. Louis calls his friend Eva Lu Ator over to help. When they examine Louis’s code, they find that he has rewritten the expmod procedure to use an explicit multiplication, rather than calling square:

; (define (expmod base exp m)
;   (cond ((= exp 0) 1)
;         ((even? exp)
;          (remainder
;           (* (expmod base (/ exp 2) m)
;              (expmod base (/ exp 2) m))
;           m))
;         (else
;          (remainder
;           (* base
;              (expmod base (- exp 1) m))
;           m))))
; “I don’t see what difference that could make,” says Louis. “I do.” says Eva. “By writing the procedure like that, you have transformed the Θ(logn)Θ(log⁡n) process into a Θ(n)Θ(n) process.” Explain.

; Because using square only compute (expmod base (/ exp 2) m) one in remainder, whereas write it seperately would dopulicate the comutation resulting.
; more specific, the it should reduce /2, but here dupulicate the *2 cancell out the bonus.
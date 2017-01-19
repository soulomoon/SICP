; Exercise 4.32: Give some examples that illustrate the difference between the streams of Chapter 3 and the “lazier” lazy lists described in this section. How can you take advantage of this extra laziness?

; both car and cdr is delayed
; which means you can define somthing like this

(define (test)
  (define a cons(c c))
  (define c 2)
  a
)

; easier for simultaneous
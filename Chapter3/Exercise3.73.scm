; Exercise 3.73: We can model electrical circuits using streams to represent the values of currents or voltages at a sequence of times. For instance, suppose we have an RC circuit consisting of a resistor of resistance RR and a capacitor of capacitance CC in series. The voltage response vv of the circuit to an injected current ii is determined by the formula in Figure 3.33, whose structure is shown by the accompanying signal-flow diagram.

 
; Figure 3.33: An RC circuit and the associated signal-flow diagram.
; Write a procedure RC that models this circuit. RC should take as inputs the values of RR, CC, and dtdt and should return a procedure that takes as inputs a stream representing the current ii and an initial value for the capacitor voltage v0v0 and produces as output the stream of voltages vv. For example, you should be able to use RC to model an RC circuit with RR = 5 ohms, CC = 1 farad, and a 0.5-second time step by evaluating (define RC1 (RC 5 1 0.5)). This defines RC1 as a procedure that takes a stream representing the time sequence of currents and an initial capacitor voltage and produces the output stream of voltages.
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")
(define (RC R C dt)
  (define (iter S v0)
    (define (integral_iter)
      (let ((i (stream-car S)))
        (cons-stream
          v0
          (add-streams 
            (scale-stream S (/ dt C)) 
            (integral_iter)))))
    (add-streams (integral_iter) (scale-stream S R)))
  iter)

(define (integral integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)

(define (RC2 R C dt)
  (define (iter S v0)
    (add-streams
      (integral (scale-stream S (/ 1 C)) v0 dt)
      (scale-stream S R)))
  iter)



(define RC1 (RC 5 1 0.5))

(display-10 (RC1 integers 1))

(define RC3 (RC2 5 1 0.5))

(display-10 (RC3 integers 1))

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; 6
; 11.5
; 18.0
; 25.5
; 34.0
; 43.5
; 54.0
; 65.5
; 78.0
; 91.5
; 106.0'done
; > 
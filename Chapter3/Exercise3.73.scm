; Exercise 3.73: We can model electrical circuits using streams to represent the values of currents or voltages at a sequence of times. For instance, suppose we have an RC circuit consisting of a resistor of resistance RR and a capacitor of capacitance CC in series. The voltage response vv of the circuit to an injected current ii is determined by the formula in Figure 3.33, whose structure is shown by the accompanying signal-flow diagram.

 
; Figure 3.33: An RC circuit and the associated signal-flow diagram.
; Write a procedure RC that models this circuit. RC should take as inputs the values of RR, CC, and dtdt and should return a procedure that takes as inputs a stream representing the current ii and an initial value for the capacitor voltage v0v0 and produces as output the stream of voltages vv. For example, you should be able to use RC to model an RC circuit with RR = 5 ohms, CC = 1 farad, and a 0.5-second time step by evaluating (define RC1 (RC 5 1 0.5)). This defines RC1 as a procedure that takes a stream representing the time sequence of currents and an initial capacitor voltage and produces the output stream of voltages.

(define (RC_transform_maker R C dt)
  (define iter)
  )
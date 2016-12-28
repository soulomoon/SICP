; Exercise 3.62: Use the results of Exercise 3.60 and Exercise 3.61 to define a procedure div-series that divides two power series. Div-series should work for any two series, provided that the denominator series begins with a nonzero constant term. (If the denominator has a zero constant term, then div-series should signal an error.) Show how to use div-series together with the result of Exercise 3.59 to generate the power series for tangent.
(load "/home/soulomoon/git/SICP/Chapter3/Exercise3.61.scm")

(define (div-series a b)
  (if (= (stream-car b) 0)
      (error "0 constant in denominator")
      (mul-series a (invert-unit-series b))))


(define s (div-series ones ones))


(display-stream 
  (stream-map (lambda (n) 
                (stream-ref s n)) 
                  (stream-enumerate-interval 0 10)))

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; 1
; 0
; 0
; 0
; 0
; 0
; 0
; 0
; 0
; 0
; 0'done
; > 
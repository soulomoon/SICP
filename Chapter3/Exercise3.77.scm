; Exercise 3.77: The integral procedure used above was analogous to the “implicit” definition of the infinite stream of integers in 3.5.2. Alternatively, we can give a definition of integral that is more like integers-starting-from (also in 3.5.2):

; (define (integral
;          integrand initial-value dt)
;   (cons-stream 
;    initial-value
;    (if (stream-null? integrand)
;        the-empty-stream
;        (integral 
;         (stream-cdr integrand)
;         (+ (* dt (stream-car integrand))
;            initial-value)
;         dt))))
; When used in systems with loops, this procedure has the same problem as does our original version of integral. Modify the procedure so that it expects the integrand as a delayed argument and hence can be used in the solve procedure shown above.
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")

; (define (integral integrand initial-value dt)
;   (cons-stream 
;    initial-value
;    (if (stream-null? integrand)
;        the-empty-stream
;        (integral 
;         (stream-cdr integrand)
;         (+ (* dt (stream-car integrand))
;            initial-value)
;         dt))))
        
(define (integral delayed_integrand initial-value dt)
  (define (integral_inner delayed_integrand initial-value)
    (cons-stream 
    initial-value
    (let ((integrand (force (force delayed_integrand))))
      (if (stream-null? integrand)
          the-empty-stream
          (integral_inner
            (delay (delay (stream-cdr integrand)))
            (+ (* dt (stream-car integrand))
              initial-value))))))
   (integral_inner delayed_integrand initial-value))

; (define (integral
;          delayed-integrand initial-value dt)
;   (define int
;     (cons-stream 
;      initial-value
;      (let ((integrand 
;             (force (force delayed-integrand))))
;        (add-streams 
;         (scale-stream integrand dt)
;         int))))
;   int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (delay (stream-map f y)))
  y)

(stream-ref  (solve (lambda (x) x) 1 0.00001) 100000)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 2.7182682371744953
; > 
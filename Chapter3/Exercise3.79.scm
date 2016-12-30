; Exercise 3.79: Generalize the solve-2nd procedure of Exercise 3.78 so that it can be used to solve general second-order differential equations d2y/dt2=f(dy/dt,y)d2y/dt2=f(dy/dt,y).
 
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")

(define (integral
         delayed-integrand initial-value dt)
  (define int
    (cons-stream 
     initial-value
     (let ((integrand 
            (force delayed-integrand)))
       (add-streams 
        (scale-stream integrand dt)
        int))))
  int)


(define (solve dt y0 dy0)
  (define dy (delay (integral y dy0 dt)))
  (define y (delay (integral dy y0 dt)))
  (force y))

(define (solve-gnd f dt y0 dy0)
  (define dy (delay (integral ddy dy0 dt)))
  (define y (delay (integral dy y0 dt)))
  (define ddy
    (delay 
      (scale-stream
        (stream-map
          f 
          (scale-stream (force dy) (/ 1 dt))
          (force y))
        (square dt))))
  (force y))
(define f 
  (lambda (dy/dt y)
    (+ dy/dt y)))

(stream-ref (solve 0.0001 1 1) 10000)
(stream-ref (solve-gnd f 0.0001 1 1) 10000)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 2.7181459268252266
; 2.0000500033321242
; > 
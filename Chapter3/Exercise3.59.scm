; Exercise 3.59: In 2.5.3 we saw how to implement a polynomial arithmetic system representing polynomials as lists of terms. In a similar way, we can work with power series, such as
; excosxsinx===1+x+12x2+13⋅2x3+14⋅3⋅2x4+…,1−12x2+14⋅3⋅2x4−…,x−13⋅2x3+15⋅4⋅3⋅2x5−…
; ex=1+x+12x2+13⋅2x3+14⋅3⋅2x4+…,cos⁡x=1−12x2+14⋅3⋅2x4−…,sin⁡x=x−13⋅2x3+15⋅4⋅3⋅2x5−…
; represented as infinite streams. We will represent the series a0+a1x+a2x2+a3x3+…a0+a1x+a2x2+a3x3+… as the stream whose elements are the coefficients a0a0, a1a1, a2a2, a3a3, ….

; The integral of the series a0+a1x+a2x2+a3x3+…a0+a1x+a2x2+a3x3+… is the series
; c+a0x+12a1x2+13a2x3+14a3x4+…,
; c+a0x+12a1x2+13a2x3+14a3x4+…,
; where cc is any constant. Define a procedure integrate-series that takes as input a stream a0a0, a1a1, a2a2, … representing a power series and returns the stream a0a0, 12a112a1, 13a213a2, … of coefficients of the non-constant terms of the integral of the series. (Since the result has no constant term, it doesn’t represent a power series; when we use integrate-series, we will cons on the appropriate constant.)
; The function x↦exx↦ex is its own derivative. This implies that exex and the integral of exex are the same series, except for the constant term, which is e0=1e0=1. Accordingly, we can generate the series for exex as
; (define exp-series
;   (cons-stream 
;    1 (integrate-series exp-series)))
; Show how to generate the series for sine and cosine, starting from the facts that the derivative of sine is cosine and the derivative of cosine is the negative of sine:

; (define cosine-series 
;   (cons-stream 1 ⟨??⟩))

; (define sine-series
;   (cons-stream 0 ⟨??⟩))
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")

; 1
(define (integrate-series s)
  (let ((coeff-stream (stream-map / ones integers)))
    (stream-map * coeff-stream s)
  )
)
; 2
(define exp-series
  (cons-stream 
   1 (integrate-series exp-series)))

(define cosine-series 
  (cons-stream 
    1 
    (stream-map 
      (lambda (x) (- 0 x))
      (integrate-series 
        sine-series))))

(define sine-series
  (cons-stream 
    0 
    (integrate-series 
      cosine-series)))

(display-stream 
  (stream-map 
    (lambda (n) 
      (stream-ref sine-series n)) 
    (stream-enumerate-interval 0 10)))

(display-stream 
  (stream-map 
    (lambda (n) 
      (stream-ref cosine-series n)) 
    (stream-enumerate-interval 0 10)))

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; 0
; 1
; 0
; -1/6
; 0
; 1/120
; 0
; -1/5040
; 0
; 1/362880
; 0'done

; 1
; 0
; -1/2
; 0
; 1/24
; 0
; -1/720
; 0
; 1/40320
; 0
; -1/3628800'done
; > 
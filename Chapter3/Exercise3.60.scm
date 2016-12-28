; Exercise 3.60: With power series represented as streams of coefficients as in Exercise 3.59, adding series is implemented by add-streams. Complete the definition of the following procedure for multiplying series:

; (define (mul-series s1 s2)
;   (cons-stream ⟨??⟩ (add-streams ⟨??⟩ ⟨??⟩)))
; You can test your procedure by verifying that sin2x+cos2x=1,sin2⁡x+cos2⁡x=1, using the series from Exercise 3.59.
(load "/home/soulomoon/git/SICP/Chapter3/Exercise3.59.scm")


(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
    (add-streams (add-streams (mul-series (stream-cdr s1) s2) 
                  (mul-series s1 (stream-cdr s2)))
                 (scale-stream 
                  (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2)))
                    -1))))

; (define s (mul-series ones ones))

(define s (add-streams (mul-series cosine-series cosine-series)
  (mul-series sine-series sine-series)))

; (display-stream 
;   (stream-map (lambda (n) 
;                 (stream-ref s n)) 
;                   (stream-enumerate-interval 0 10)))

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
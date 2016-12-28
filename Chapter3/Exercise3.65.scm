; Exercise 3.65: Use the series
; ln2=1−12+13−14+…
; ln⁡2=1−12+13−14+…
; to compute three sequences of approximations to the natural logarithm of 2, in the same way we did above for ππ. How rapidly do these sequences converge?
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")

(define (ln2-summands n)
  (cons-stream 
   (/ 1.0 n)
   (stream-map - (ln2-summands (+ n 1)))))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))     ; Sₙ₋₁
        (s1 (stream-ref s 1))     ; Sₙ
        (s2 (stream-ref s 2)))    ; Sₙ₊₁
    (cons-stream 
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream 
   s
   (make-tableau
    transform
    (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))


(define ln2-stream
   (partial-sums (ln2-summands 1)))
(define eln2-stream (euler-transform ln2-stream))
(define aln2-stream
  (accelerated-sequence euler-transform
                       ln2-stream))


(display-10 ln2-stream)
(display-10 eln2-stream)
(display-10 aln2-stream)


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; 1.0
; 0.5
; 0.8333333333333333
; 0.5833333333333333
; 0.7833333333333332
; 0.6166666666666666
; 0.7595238095238095
; 0.6345238095238095
; 0.7456349206349207
; 0.6456349206349207'done

; 0.7
; 0.6904761904761905
; 0.6944444444444444
; 0.6924242424242424
; 0.6935897435897436
; 0.6928571428571428
; 0.6933473389355742
; 0.6930033416875522
; 0.6932539682539683
; 0.6930657506744464'done

; 1.0
; 0.7
; 0.6932773109243697
; 0.6931488693329254
; 0.6931471960735491
; 0.6931471806635636
; 0.6931471805604039
; 0.6931471805599445
; 0.6931471805599427
; 0.6931471805599454'done
; > 
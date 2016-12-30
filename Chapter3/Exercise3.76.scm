; Exercise 3.75: Unfortunately, Alyssa’s zero-crossing detector in Exercise 3.74 proves to be insufficient, because the noisy signal from the sensor leads to spurious zero crossings. Lem E. Tweakit, a hardware specialist, suggests that Alyssa smooth the signal to filter out the noise before extracting the zero crossings. Alyssa takes his advice and decides to extract the zero crossings from the signal constructed by averaging each value of the sense data with the previous value. She explains the problem to her assistant, Louis Reasoner, who attempts to implement the idea, altering Alyssa’s program as follows:

; (define (make-zero-crossings 
;          input-stream last-value)
;   (let ((avpt 
;          (/ (+ (stream-car input-stream) 
;                last-value) 
;             2)))
;     (cons-stream     
;      (sign-change-detector avpt last-value)
;      (make-zero-crossings 
;       (stream-cdr input-stream) avpt))))
; This does not correctly implement Alyssa’s plan. Find the bug that Louis has installed and fix it without changing the structure of the program. (Hint: You will need to increase the number of arguments to make-zero-crossings.)
(load "/home/soulomoon/git/SICP/Chapter3/Exercise3.74.scm")

(define (make-zero-crossings input-stream smooth)
  (let ((smoothed_stream (smooth input-stream)))
    (stream-map 
      sign-change-detector 
        smoothed_stream
        (cons-stream (stream-car smoothed_stream) smoothed_stream))))

(define (smooth s)
  (stream-map (lambda (a b) (/ (+ a b) 2)) 
    s
    (cons-stream (stream-car s) s)))
(define a (make-zero-crossings sense-data smooth))

; (display-10 a)



; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; 0
; 0
; 0
; 0
; 0
; -1
; 0
; 0
; 0
; 0
; 1'done

; 0
; 0
; 0
; 0
; 0
; 0
; -1
; 0
; 0
; 0
; 0'done
; > 
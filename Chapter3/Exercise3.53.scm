; Exercise 3.53: Without running the program, describe the elements of the stream defined by
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")
(define (add-streams s1 s2) 
  (stream-map + s1 s2))
; (define s (cons-stream 1 (add-streams s s)))
(define s (cons-stream 1 (add-streams s s)))
; 1 2 4 8 16 just as 
; (define double 
;   (cons-stream 1 (scale-stream double 2)))

(stream-ref s 10)


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 1024
; > 
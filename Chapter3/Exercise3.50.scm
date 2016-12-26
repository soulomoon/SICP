; Exercise 3.50: Complete the following definition, which generalizes stream-map to allow procedures that take multiple arguments, analogous to map in 2.2.1, Footnote 78.

; (define (stream-map proc . argstreams)
;   (if (⟨??⟩ (car argstreams))
;       the-empty-stream
;       (⟨??⟩
;        (apply proc (map ⟨??⟩ argstreams))
;        (apply stream-map
;               (cons proc 
;                     (map ⟨??⟩ 
;                          argstreams))))))
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc 
                    (map stream-cdr 
                         argstreams))))))

(define a (stream-enumerate-interval 10 20))
(define b (stream-enumerate-interval 10 20))

(define c (stream-map + a b a))

(display-stream c)


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; 30
; 33
; 36
; 39
; 42
; 45
; 48
; 51
; 54
; 57
; 60'done
; > 
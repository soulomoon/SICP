; Exercise 3.72: In a similar way to Exercise 3.71 generate a stream of all numbers that can be written as the sum of two squares in three different ways (showing how they can be so written).
(load "/home/soulomoon/git/SICP/Chapter3/Exercise3.71.scm")

(define (three_Ramanujan_iter S)
  (let ((first (stream-car S))
        (rest (stream-cdr S)))
        (let ((second (stream-car rest))
              (third (stream-car (stream-cdr rest))))
          (if (= (Ramanujan_weight first) 
                 (Ramanujan_weight second) 
                 (Ramanujan_weight third))
              (cons-stream
                (list first second third (Ramanujan_weight first))
                (three_Ramanujan_iter rest))
              (three_Ramanujan_iter rest)))))

(define three_Ramanujan_numbers 
  (three_Ramanujan_iter Ramanujan_weighted_stream))

(display-10 three_Ramanujan_numbers)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; ((167 436) (228 423) (255 414) 87539319)
; ((11 493) (90 492) (346 428) 119824488)
; ((111 522) (359 460) (408 423) 143604279)
; ((70 560) (198 552) (315 525) 175959000). . user break
; > 
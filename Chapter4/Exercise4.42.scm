; Exercise 4.42: Solve the following “Liars” puzzle (from Phillips 1934):

; Five schoolgirls sat for an examination. Their parents—so they thought—showed an undue degree of interest in the result. They therefore agreed that, in writing home about the examination, each girl should make one true statement and one untrue one. The following are the relevant passages from their letters:

; Betty: “Kitty was second in the examination. I was only third.”
; Ethel: “You’ll be glad to hear that I was on top. Joan was second.”
; Joan: “I was third, and poor old Ethel was bottom.”
; Kitty: “I came out second. Mary was only fourth.”
; Mary: “I was fourth. Top place was taken by Betty.”
; What in fact was the order in which the five girls were placed?
#lang racket
(require swindle/extra)
(require sicp)
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (differ) 
  (amb 
    (lambda (x y) (and (not x) y)) 
    (lambda (x y) (and x (not y)))))
(define (require p)
  (if (not p) (amb)))
(define (solve-liars)
  (let ((Betty (amb 1 2 3 4 5))
        (Ethel (amb 1 2 3 4 5))
        (Joan (amb 1 2 3 4 5))
        (Kitty (amb 1 2 3 4 5))
        (Mary (amb 1 2 3 4 5)))
        (let ((statement_list
                (list
                  (list (= 2 Kitty) (= 3 Betty))
                  (list (= 1 Ethel) (= 2 Joan))
                  (list (= 3 Joan) (= 5 Ethel))
                  (list (= 2 Kitty) (= 4 Mary))
                  (list (= 4 Mary) (= 1 Betty)))))
              (map 
                (lambda (statement) 
                  (require (apply (differ) statement)))
                statement_list)
              (require (distinct? (list Betty Ethel Joan Kitty Mary)))
              
              (map list 
                (list 'Betty 'Ethel 'Joan 'Kitty 'Mary)
                (list Betty Ethel Joan Kitty Mary)))))
                

(display (amb-collect (solve-liars)))


; Welcome to DrRacket, version 6.7 [3m].
; Language: racket, with debugging; memory limit: 512 MB.
; {((Betty 3) (Ethel 5) (Joan 2) (Kitty 1) (Mary 4))}
; > 


; it is fun, here is even an more interesting version, but it takes a lot longer,
; kills your cpu right at this moment.

; (define (solve-liars)
;   (let ((Betty (amb 1 2 3 4 5))
;         (Ethel (amb 1 2 3 4 5))
;         (Joan (amb 1 2 3 4 5))
;         (Kitty (amb 1 2 3 4 5))
;         (Mary (amb 1 2 3 4 5)))
;     (define (statement_list)
;       (list
;        (amb (= 2 Kitty) (= 3 Betty))
;        (amb (= 1 Ethel) (= 2 Joan))
;        (amb (= 3 Joan) (= 5 Ethel))
;        (amb (= 2 Kitty) (= 4 Mary))
;        (amb (= 4 Mary) (= 1 Betty))))
                            
;     (map 
;      (lambda (statement1 statement2) 
;        (require (and statement1 (not statement2))))
;      (statement_list) (statement_list))
    
;     (require (distinct? (list Betty Ethel Joan Kitty Mary)))
;     (map list 
;          (list 'Betty 'Ethel 'Joan 'Kitty 'Mary)
;          (list Betty Ethel Joan Kitty Mary))))
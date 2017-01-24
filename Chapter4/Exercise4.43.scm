; Exercise 4.43: Use the amb evaluator to solve the following puzzle:253

; Mary Ann Moore’s father has a yacht and so has each of his four friends: Colonel Downing, Mr. Hall, Sir Barnacle Hood, and Dr. Parker. Each of the five also has one daughter and each has named his yacht after a daughter of one of the others. Sir Barnacle’s yacht is the Gabrielle, Mr. Moore owns the Lorna; Mr. Hall the Rosalind. The Melissa, owned by Colonel Downing, is named after Sir Barnacle’s daughter. Gabrielle’s father owns the yacht that is named after Dr. Parker’s daughter. Who is Lorna’s father?

; Try to write the program so that it runs efficiently (see Exercise 4.40). Also determine how many solutions there are if we are not told that Mary Ann’s last name is Moore.
; #lang racket
; (require swindle/extra)
; (require sicp)
#lang swindle
(define (require p)
  (if (not p) (amb)))
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (amb_name)
  (amb 'Gabrielle 'Lorna 'Rosalind 'Melissa 'Mary))
(define (amb_unit)
  (cons (amb_name) (amb_name)))

(define (solve_daughter_yacht1)
  (let ((Moore (amb_unit)))
    (require (eq? (car Moore) 'Mary))
    (require (eq? (cdr Moore) 'Lorna))
    (let ((Barnacle (amb_unit)))
      (require (eq? (cdr Barnacle) 'Gabrielle))
      (require (eq? (car Barnacle) 'Melissa))
      (let ((Downing (amb_unit)))
        (require (eq? (cdr Downing) 'Melissa))
        (let ((Hall (amb_unit)))
          (require (eq? (cdr Hall) 'Rosalind))
          (let ((Parker (amb_unit)))
            (let ((families 
                   (list Moore Downing Hall Barnacle Parker)))
              (require (distinct? (map car families)))
              (require (distinct? (map cdr families)))
              (for-each 
               (lambda (family) (require (not (eq? (car family) (cdr family)))))families)
              (for-each 
               (lambda (family) 
                 (require 
                   (if (eq? 'Gabrielle (car family))
                       (eq? (cdr family) (car Parker))
                       true)))
               families)
               
              (map list (list 'Moore 'Downing 'Hall 'Barnacle 'Parker) families))))))))
; if we don't know Mary Ann's father is Moore
(define (solve_daughter_yacht2)
  (let ((Moore (amb_unit)))
    ; (require (eq? (car Moore) 'Mary))
    (require (eq? (cdr Moore) 'Lorna))
    (let ((Barnacle (amb_unit)))
      (require (eq? (cdr Barnacle) 'Gabrielle))
      (require (eq? (car Barnacle) 'Melissa))
      (let ((Downing (amb_unit)))
        (require (eq? (cdr Downing) 'Melissa))
        (let ((Hall (amb_unit)))
          (require (eq? (cdr Hall) 'Rosalind))
          (let ((Parker (amb_unit)))
            (let ((families 
                   (list Moore Downing Hall Barnacle Parker)))
              (require (distinct? (map car families)))
              (require (distinct? (map cdr families)))
              (for-each 
               (lambda (family) (require (not (eq? (car family) (cdr family)))))families)
              (for-each 
               (lambda (family) 
                 (require 
                   (if (eq? 'Gabrielle (car family))
                       (eq? (cdr family) (car Parker))
                       true)))
               families)
               
              (map list (list 'Moore 'Downing 'Hall 'Barnacle 'Parker) families))))))))

              
(display (amb-collect (solve_daughter_yacht1)))
(newline )
(for-each (lambda (x) (newline ) (display x)) (amb-collect (solve_daughter_yacht2)))


; Welcome to DrRacket, version 6.7 [3m].
; Language: swindle, with debugging; memory limit: 1024 MB.
; (((Moore (Mary . Lorna)) (Downing (Lorna . Melissa)) (Hall (Gabrielle . Rosalind)) (Barnacle (Melissa . Gabrielle)) (Parker (Rosalind . Mary))))

; ((Moore (Gabrielle . Lorna)) (Downing (Rosalind . Melissa)) (Hall (Mary . Rosalind)) (Barnacle (Melissa . Gabrielle)) (Parker (Lorna . Mary)))
; ((Moore (Mary . Lorna)) (Downing (Lorna . Melissa)) (Hall (Gabrielle . Rosalind)) (Barnacle (Melissa . Gabrielle)) (Parker (Rosalind . Mary)))
; > 
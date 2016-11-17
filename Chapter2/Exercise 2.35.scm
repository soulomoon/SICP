; Exercise 2.35: Redefine count-leaves from 2.2.2 as an accumulation:

; (define (count-leaves t)
;   (accumulate ⟨??⟩ ⟨??⟩ (map ⟨??⟩ ⟨??⟩)))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))


(define (count-leaves t)
  (accumulate 
                (lambda (x y) 
                    (+ x y)
                ) 
                0 
                (map (lambda (subtree) 
                        (if (pair? subtree)
                            (count-leaves subtree)
                            1
                        )
                     ) 
                    t)))

(define x (cons (list 1 2) (list 3 4)))

(count-leaves x)
(count-leaves (list x x))


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 4
; 8
; > 
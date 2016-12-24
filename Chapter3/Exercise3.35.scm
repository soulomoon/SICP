; Exercise 3.35: Ben Bitdiddle tells Louis that one way to avoid the trouble in Exercise 3.34 is to define a squarer as a new primitive constraint. Fill in the missing portions in Ben’s outline for a procedure to implement such a constraint:

; (define (squarer a b)
;   (define (process-new-value)
;     (if (has-value? b)
;         (if (< (get-value b) 0)
;             (error "square less than 0: 
;                     SQUARER" 
;                    (get-value b))
;             ⟨alternative1⟩)
;         ⟨alternative2⟩))
;   (define (process-forget-value) ⟨body1⟩)
;   (define (me request) ⟨body2⟩)
;   ⟨rest of definition⟩
;   me)
(load "/home/soulomoon/git/SICP/Chapter3/constraint.scm")
(define (square x) (* x x))
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: 
                    SQUARER" 
                   (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (square (get-value a)) me))))
  (define (process-forget-value) 
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request: 
                   squarer" 
                  request))))
  (connect a me)
  (connect b me)
  me)


(define a (make-connector))
(probe 'a a)
(define b (make-connector))
(probe 'b b)

(squarer a b)
(set-value! a 1 'user)
(forget-value! a 'user)
(set-value! b 10 'user)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; #<procedure:...3/constraint.scm:102:2>
; #<procedure:...3/constraint.scm:102:2>
; #<procedure>

; Probe: a = 1
; Probe: b = 1'done

; Probe: a = ?
; Probe: b = ?'done

; Probe: b = 10
; Probe: a = 3.1622776601683795'done
; > 
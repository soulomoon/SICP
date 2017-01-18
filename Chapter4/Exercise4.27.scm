; Exercise 4.27: Suppose we type in the following definitions to the lazy evaluator:

; (define count 0)
; (define (id x) (set! count (+ count 1)) x)
; Give the missing values in the following sequence of interactions, and explain your answers.242

; (define w (id (id 10)))

; ;;; L-Eval input:
; count

; ;;; L-Eval value:
; ⟨response⟩

; ;;; L-Eval input:
; w

; ;;; L-Eval value:
; ⟨response⟩

; ;;; L-Eval input:
; count

; ;;; L-Eval value:
; ⟨response⟩

(define count 0)
(define (id x) (set! count (+ count 1)) x)

;;; L-Eval input:
count

;;; L-Eval value:
1

;;; L-Eval input:
w

;;; L-Eval value:
10

;;; L-Eval input:
count

;;; L-Eval value:
2


; (interpret
; '(begin
; (define count 0)
; (define (id x) (set! count (+ count 1)) x)
; (define w (id (id 10)))
; count
; )
; )

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'done
; eval#-----(begin (define count 0) (define (id x) (set! count (+ count 1)) x) (define w (id (id 10))) count (display w) count)
; eval#-----(define count 0)
; eval#-----0
; eval#-----(define (id x) (set! count (+ count 1)) x)
; eval#-----(lambda (x) (set! count (+ count 1)) x)
; eval#-----(define w (id (id 10)))
; eval#-----(id (id 10))
; eval#-----id
; delay-it--------(id 10)
; eval#-----(set! count (+ count 1))
; eval#-----(+ count 1)
; eval#-----+
; eval#-----count
; eval#-----1
; eval#-----x
; eval#-----count
; eval#-----(display w)
; eval#-----display
; eval#-----w
; eval#-----(id 10)
; eval#-----id
; delay-it--------10
; eval#-----(set! count (+ count 1))
; eval#-----(+ count 1)
; eval#-----+
; eval#-----count
; eval#-----1
; eval#-----x
; eval#-----10
; force-it------thunk10
; force-it------thunk(id 10)
; 10eval#-----count
; 2
; > 
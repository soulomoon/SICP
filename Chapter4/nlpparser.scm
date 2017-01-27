(load "/Users/soulomoon/git/SICP/Chapter4/zch4-ambeval.scm")
(interpret '(define (require p)
  (if (not p) (amb))))
(interpret '(define nouns 
'(noun student professor cat class)))

(interpret '(define verbs 
'(verb studies lectures eats sleeps)))
(interpret '(define prepositions 
'(prep for to in by with)))
(interpret '(define articles '(article the a)))

; (interpret '(define (parse-sentence)
;   (list 'sentence
;          (parse-noun-phrase)
;          (parse-word verbs)))

; (interpret '(define (parse-noun-phrase)
;   (list 'noun-phrase
;         (parse-word articles)
;         (parse-word nouns)))

(interpret '(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) 
                 (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
(list (car word-list) found-word))))

(interpret '(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase))))

(interpret '(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase))))

(interpret '(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb 
     verb-phrase
     (maybe-extend 
      (list 'verb-phrase
            verb-phrase
            (parse-prepositional-phrase)))))
(maybe-extend (parse-word verbs))))

(interpret '(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb 
     noun-phrase
     (maybe-extend 
      (list 'noun-phrase
            noun-phrase
            (parse-prepositional-phrase)))))
(maybe-extend (parse-simple-noun-phrase))))

(interpret '(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
(parse-word nouns))))

(interpret '(define *unparsed* '()))

(interpret '(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent)))


(define (make-interpret)
  (let ((try false))
        (define (interpret-rm input)
          (newline )
          (if (eq? input 'try-again)
              (try)
          (ambeval 
           input
           the-global-environment
           ;; ambeval success
           (lambda (val next-alternative)
             (set! try next-alternative)
             val)
           ;; ambeval failure
           (lambda ()
             (newline)
             (display "fail")
             (newline))))
          )
        interpret-rm
  )
)
(define interpret (make-interpret))

(define (try-again) (newline )(interpret 'try-again))
; (driver-loop)
; (interpret '(display (parse '(the professor lectures to the student in the class with the cat))))
; (newline )
; (interpret 'try-again)
; (display (parse '(the cat eats)))

; (display (parse '(the professor lectures to 
;          the student with the cat)))
;(parse '(the professor lectures to the student in the class with the cat)))


; (for-each (lambda (x) (newline ) (display x)) (amb-collect (parse '(the professor lectures to the student in the class with the cat))))
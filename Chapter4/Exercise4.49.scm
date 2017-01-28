; Exercise 4.49: Alyssa P. Hacker is more interested in generating interesting sentences than in parsing them. She reasons that by simply changing the procedure parse-word so that it ignores the “input sentence” and instead always succeeds and generates an appropriate word, we can use the programs we had built for parsing to do generation instead. Implement Alyssa’s idea, and show the first half-dozen or so sentences generated.258

(load "/Users/soulomoon/git/SICP/Chapter4/nlpparser.scm")

(interpret '(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) 
       (an-element-of (cdr items)))))



(interpret '(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) 
                 (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
(list (car word-list) (an-element-of (cdr word-list))))))

(interpret '(display (parse '(the professor lectures to the student in the class with the cat))))

(try-again)
(try-again)
(try-again)
(try-again)
(try-again)
(try-again)
(try-again)
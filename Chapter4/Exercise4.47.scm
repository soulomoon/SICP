; Exercise 4.47: Louis Reasoner suggests that, since a verb phrase is either a verb or a verb phrase followed by a prepositional phrase, it would be much more straightforward to define the procedure parse-verb-phrase as follows (and similarly for noun phrases):

; (define (parse-verb-phrase)
;   (amb (parse-word verbs)
;        (list 
;         'verb-phrase
;         (parse-verb-phrase)
;         (parse-prepositional-phrase))))
; Does this work? Does the program’s behavior change if we interchange the order of expressions in the amb?

it is an infinite loop - -// (eval from left to right)
if it is from right to left, it can stop, but the behavior still wrong
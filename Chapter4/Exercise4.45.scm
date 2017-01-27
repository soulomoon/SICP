; Exercise 4.45: With the grammar given above, the following sentence can be parsed in five different ways: “The professor lectures to the student in the class with the cat.” Give the five parses and explain the differences in shades of meaning among them.

(load "/Users/soulomoon/git/SICP/Chapter4/nlpparser.scm")

(interpret '(display (parse '(the professor lectures to the student in the class with the cat))))

(try-again)
(try-again)
(try-again)
(try-again)
(try-again)
; pvp->3ppp->each to a pnp->psnp (1)
; pvp->2ppp->each to a pnp-> 1:psnp 2:psnp,ppp->psnp,pnp->psnp,(2)
; pvp->ppp->pnp->psn,2ppp->each to pnp->psnp(1)
; pvp->ppp->pnp->psn,ppp->pnp->psnp,ppp->pnp->psnp(1)

; total 5 of them.

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.

; (sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

; (sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep in) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))

; (sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))))) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

; (sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))

; (sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep in) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))))

; Exercise 4.68: Define rules to implement the reverse operation of Exercise 2.18, which returns a list containing the same elements as a given list in reverse order. (Hint: Use append-to-form.) Can your rules answer both (reverse (1 2 3) ?x) and (reverse ?x (1 2 3))?

(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")

(inssert_to_init
'(

  (rule (append-to-form () ?y ?y))
  (rule (append-to-form (?u . ?v) ?y (?u . ?z))
        (append-to-form ?v ?y ?z))

  (rule (last-pair (?x) (?x)))
  (rule (last-pair (?v . ?z) (?x))
        (last-pair ?z (?x)))


  (rule (reverse (?a ?b) (?b ?a)))
  (rule (reverse (?a ?b ?c) (?c ?b ?a)))
  (rule (reverse (?a) (?a)))
  (rule (reverse ?a (?c ?c1 . ?d))
        (and
        (same ?d (?k0 ?k1 . ?g)) 
        ; (append-to-form ?list1 (?a) (?c . ?d))
        (append-to-form ?list2 (?c1 ?c) ?a)
        (reverse ?list2 ?d)
        ; (reverse ?list1 (?c . ?d))
        ; (reverse ?list1 ?b)
        
        )
  )
)
(inqu
'(
 (reverse (2 3) ?x)
 (reverse (1 2 3) ?x)
 (reverse ?x (1 2 3))
 (reverse (1 2 3 4) ?x)
 (reverse ?x (1 2 3 4))
)
)
; manage to get the answer working by hand, but with the case there are more numbers, 
; right side variable get a answer and stuck, need to implement detector for infinite loop

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.
; 'done


; (reverse (2 3) (3 2))


; (reverse (1 2 3) (3 2 1))


; (reverse (3 2 1) (1 2 3))


; (reverse (1 2 3 4) (4 3 2 1))


; (reverse (4 3 2 1) (1 2 3 4)). . git/SICP/Chapter4/ch4-query.rkt:468:2: user break
; > 
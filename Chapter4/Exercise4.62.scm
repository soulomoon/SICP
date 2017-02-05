; Exercise 4.62: Define rules to implement the last-pair operation of Exercise 2.17, which returns a list containing the last element of a nonempty list. Check your rules on queries such as (last-pair (3) ?x), (last-pair (1 2 3) ?x) and (last-pair (2 ?x) (3)). Do your rules work correctly on queries such as (last-pair ?x (3))?

(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")

(inssert_to_init '(
  (rule (last-pair ?x (?v . ?x))
        (not (last-pair ?l ?x)))
))

(inqu'(

(and (last-pair ?x (1 2 3))
(last-pair ?y ?x))
))
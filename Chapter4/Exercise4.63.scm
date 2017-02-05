; Exercise 4.63: The following data base (see Genesis 4) traces the genealogy of the descendants of Ada back to Adam, by way of Cain:

; (son Adam Cain) (son Cain Enoch)
; (son Enoch Irad) (son Irad Mehujael)
; (son Mehujael Methushael)
; (son Methushael Lamech)
; (wife Lamech Ada) (son Ada Jabal)
; (son Ada Jubal)
; Formulate rules such as “If SS is the son of ff, and ff is the son of GG, then SS is the grandson of GG” and “If WW is the wife of MM, and SS is the son of WW, then SS is the son of MM” (which was supposedly more true in biblical times than today) that will enable the query system to find the grandson of Cain; the sons of Lamech; the grandsons of Methushael. (See Exercise 4.69 for some rules to deduce more complicated relationships.)

(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")

(inssert_to_init '(
  (son Adam Cain) (son Cain Enoch)
  (son Enoch Irad) (son Irad Mehujael)
  (son Mehujael Methushael)
  (son Methushael Lamech)
  (wife Lamech Ada) (son Ada Jabal)
  (son Ada Jubal)
  
  (rule (grandson ?a ?c)
        (and (son ?a ?b)
             (son ?b ?c)))
  (rule (son ?a ?c)
        (and (wife ?a ?b)
              (son ?b ?c)))
  ))
(inqu'(
  (son ?x ?y)
  (grandson ?x ?y)
))

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.
; 'done


; (son Adam Cain)
; (son Cain Enoch)
; (son Enoch Irad)
; (son Irad Mehujael)
; (son Mehujael Methushael)
; (son Methushael Lamech)
; (son Ada Jabal)
; (son Ada Jubal)
; (son Lamech Jabal)
; (son Lamech Jubal)


; (grandson Adam Enoch)
; (grandson Cain Irad)
; (grandson Enoch Mehujael)
; (grandson Irad Methushael)
; (grandson Mehujael Lamech)
; (grandson Methushael Jabal)
; (grandson Methushael Jubal)
; > 
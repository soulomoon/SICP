; Exercise 4.59: Ben Bitdiddle has missed one meeting too many. Fearing that his habit of forgetting meetings could cost him his job, Ben decides to do something about it. He adds all the weekly meetings of the firm to the Microshaft data base by asserting the following:

; (meeting accounting (Monday 9am))
; (meeting administration (Monday 10am))
; (meeting computer (Wednesday 3pm))
; (meeting administration (Friday 1pm))
; Each of the above assertions is for a meeting of an entire division. Ben also adds an entry for the company-wide meeting that spans all the divisions. All of the company’s employees attend this meeting.

; (meeting whole-company (Wednesday 4pm))
; On Friday morning, Ben wants to query the data base for all the meetings that occur that day. What query should he use?
; Alyssa P. Hacker is unimpressed. She thinks it would be much more useful to be able to ask for her meetings by specifying her name. So she designs a rule that says that a person’s meetings include all whole-company meetings plus all meetings of that person’s division. Fill in the body of Alyssa’s rule.
; (rule (meeting-time ?person ?day-and-time)
;       ⟨rule-body⟩)
; Alyssa arrives at work on Wednesday morning and wonders what meetings she has to attend that day. Having defined the above rule, what query should she make to find this out?

(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")

(iqe
'(assert! 
  (meeting accounting (Monday 9am)))
'(assert! 
  (meeting administration (Monday 10am)))
'(assert! 
  (meeting computer (Wednesday 3pm)))
'(assert! 
  (meeting administration (Friday 1pm)))
'(assert! 
  (meeting whole-company (Wednesday 4pm)))

; 1
'(meeting ?div (Friday ?am))
; 2
'(assert! 
  (rule (meeting-time ?person ?day-and-time)
        (and 
          [job ?person (?div . ?x)]
          (or 
            [meeting ?div ?day-and-time]
            [meeting whole-company ?day-and-time]
          )
        ))
        )

'(meeting-time (Hacker Alyssa P) ?t)

; 3
'(and 
  [meeting ?div (Wednesday ?am)]
  [meeting-time (Hacker Alyssa P) (Wednesday ?am)]
  )
)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.

; Assertion added to data base.
; Assertion added to data base.
; Assertion added to data base.
; Assertion added to data base.
; Assertion added to data base.

; (meeting administration (Friday 1pm))

; Assertion added to data base.

; (meeting-time (Hacker Alyssa P) (Wednesday 3pm))
; (meeting-time (Hacker Alyssa P) (Wednesday 4pm))


; (and (meeting whole-company (Wednesday 4pm)) (meeting-time (Hacker Alyssa P) (Wednesday 4pm)))
; (and (meeting computer (Wednesday 3pm)) (meeting-time (Hacker Alyssa P) (Wednesday 3pm)))
; > 
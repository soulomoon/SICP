; Exercise 1.36: Modify fixed-point so that it prints the sequence of approximations it generates, using the newline and display primitives shown in Exercise 1.22. Then find a solution to xx=1000xx=1000 by finding a fixed point of x↦log(1000)/log(x)x↦log⁡(1000)/log⁡(x). (Use Scheme’s primitive log procedure, which computes natural logarithms.) Compare the number of steps this takes with and without average damping. (Note that you cannot start fixed-point with a guess of 1, as this would cause division by log(1)=0log⁡(1)=0.)
#lang planet neil/sicp

(define tolerence 0.00001)

(define (fixedPoint f first_guess)
    (define (closeEnough x y)
        (let ((dis (abs (- x y))))
            (if (< dis tolerence)
                true
                false
            )
        )
    )
    (define (try old_guess)
        (display old_guess)
        (newline)
        (let ((new_guess (f old_guess)))
            (if (closeEnough new_guess old_guess)
                new_guess
                (try new_guess))
        )
    )
    (try first_guess)
)

(define (logxx_approach x)
    (/
        (log 1000)
        (log x)
    )
)

(define (logxx_approach_average x)
  (define (average x y)
    (/ (+ x y) 2))
  (let ((new_x (/ (log 1000) (log x))))
    (average x new_x))
)

(fixedPoint logxx_approach 120)
(display "*********************")
(fixedPoint logxx_approach_average 120)

Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
120
1.442875653915591
18.84080023510588
2.3527578553654105
8.073691660179224
3.307344370515809
5.775012252839923
3.939319199726363
5.038450322708185
4.2716971330516165
4.757370510070046
4.428913876312149
4.641827067701032
4.499849547414439
4.592788144553528
4.531199030747006
4.571685360642406
4.5449288962745396
4.562549636151627
4.55091839051009
4.558584321080103
4.553526758650298
4.5568612457504925
4.5546618307541795
4.55611213729376
4.555155614746718
4.555786392005102
4.555370392469622
4.555644730440119
4.555463807075625
4.555583121530996
4.555504435383807
4.5555563272163155
4.55552210542835
4.555544674018586
4.555529790436921
4.555539605873666
*********************120
60.721437826957796
31.201836299107846
16.60481315913145
9.531679004668709
6.297750106617641
5.025786161902264
4.652070819132921
4.572733939660905
4.558488573812262
4.556039189832051
4.55562144942426
4.555550304567164
4.55553819089232
4.555536128408786
>
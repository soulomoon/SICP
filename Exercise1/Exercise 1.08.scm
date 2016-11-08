#lang planet neil/sicp
;Exercise 1.8: Newton’s method for cube roots is based on the fact that if yy is an approximation to the cube root of xx, then a better approximation is given by the value
;(x/y2+2y)/3
;Use this formula to implement a cube-root procedure analogous to the square-root procedure.(In 1.3.4 we will see how to implement Newton’s method in general as an abstraction of these square-root and cube-root procedures.)

(define (cube x)
  (* x x x))
(cube 10)

(define (improve y x)
  (/ (+ (/ x (* y y))
        (* 2 y)) 3))

(improve 10 1000)

(define (abs x)
  (if (> x 0)
      x
      (- 0 x))
  )
(abs -110)

(define (goodEnough? guess x)
  (< (abs(- (cube guess) x)) 0.001))

(goodEnough? 1011 1000)

(define (cuberoot-itr guess x)
  (if (goodEnough? guess x)
      guess
      (cuberoot-itr (improve guess x) x)))

(cuberoot-itr 101 1000)

(define (cubeRoot guess) (cuberoot-itr 13 guess))
(cubeRoot 1000)

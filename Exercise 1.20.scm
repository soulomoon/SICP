; Exercise 1.20. The process that a procedure generates is of course dependent on the rules used by the interpreter. As an example, consider the iterative gcd procedure given above. Suppose we were to interpret this procedure using normal-order evaluation, as discussed in section 1.1.5. (The normal-order-evaluation rule for if is described in exercise 1.5.) Using the substitution method (for normal order), illustrate the process generated in evaluating (gcd 206 40) and indicate the remainder operations that are actually performed. How many remainder operations are actually performed in the normal-order evaluation of (gcd 206 40)? In the applicative-order evaluation?

normal-order evaluation:
(gcd 206 40)
(gcd 40 (remainder 206 40)) 1
(gcd 40 6)
(gcd 6 (remainder 40 6)) 2
(gcd 6 4)
(gcd 4 (remainder 6 4)) 3
(gcd 4 2)
(gcd 4 (remainder 4 2)) 4
(gcd 4 0)
4


applicative-order evaluation:
for (gcd a b)
in the place of a and b:
a grow as b,
but one move behind.

everytime step we evaluat b one time.
until it is 0

so we count each time how much steps it takes to evaluat b

new b equals (remainder a b)
takes the new steps to be step(newB) = step(a) + step(b) + 1
we have x(n+2) = x(n+1) + x(n)
begine from when
x(0) = 1, x(-1) = 0
x(1) = 1
x(2) = 3
x(3) = 5
until x(4) = 9
s(1~4) = 18


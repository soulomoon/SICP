; Exercise 2.13: Show that under the assumption of small percentage tolerances there is a simple formula for the approximate percentage tolerance of the product of two intervals in terms of the tolerances of the factors. You may simplify the problem by assuming that all numbers are positive.

let
tx = x * px
ty = y * py

(x + tx)(y + ty)

x*y + y*tx + x*ty + tx*ty

we have：
x*y + tx*ty +- (x*ty + y*tx)

x*y + tx*ty +- x*y(px+py)

x*y(1 + px*py) +- x*y(px+py)

pxy = (px+py)/(1 + px*py)

~ (px+py)
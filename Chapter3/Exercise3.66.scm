; Exercise 3.66: Examine the stream (pairs integers integers). Can you make any general comments about the order in which the pairs are placed into the stream? For example, approximately how many pairs precede the pair (1, 100)? the pair (99, 100)? the pair (100, 100)? (If you can make precise mathematical statements here, all the better. But feel free to give more qualitative answers if you find yourself getting bogged down.)

(1, 100) preceding first there are (1, 1) to (1, 99)
total 99, then observing the interleave, there would be 98 with form of (n, m),
where n > 1, m < 99.
total 197
with total of m pick, 
approximately.
1/2:(1, n)
1/4:(2, n) 
1/8:(3, n)
1/2^k:(k, n)
because the first is (1, 1)
and there are not coresponding (2, 1)
then you have to count for (n, 1)'s location
every time step into the next, you have to in pair stage take two step
for nth pair one step means 2^(n-1)
(1, 1): 1
(2, 1): 2 * 1 + 1
(3, 1): 2 * 2 + 2 * 1 + 1
(n ,1): 2^n - 1

for (n, m)'location, take step too,
you need two step to get to the next m, except for the second one , which you only need one step to get to
(n, 2): 2^n - 1 + 2^(n-1)
(n ,3): 2^n - 1 + 2^(n-1) +2^n
(n, m): (m-1) * 2^n + 2^(n-1) -1

conclude:
(n, 1): 2^n -1
(n, m): m * 2^n - 2^(n-1) -1


so the total preceding pairs:
for (1, 100): 100*2 - 2 - 1 = 197
for (99, 100): 100*2^99 - 2^98 - 2
for (100, 100):100*2^100 - 2^99 - 2




; Exercise 4.72: Why do disjoin and stream-flatmap interleave the streams rather than simply append them? Give examples that illustrate why interleaving works better. (Hint: Why did we use interleave in 3.5.3?)

because disjoin is have two match,  when we interleave it, either result come out one then another.
for infinite case, it would be able to list all posibility
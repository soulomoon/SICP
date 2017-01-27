; Exercise 4.46: The evaluators in 4.1 and 4.2 do not determine what order operands are evaluated in. We will see that the amb evaluator evaluates them from left to right. Explain why our parsing program wouldn’t work if the operands were evaluated in some other order.

because the it use set!, it relies on the order. and we have written it to be left to right, other order won't work
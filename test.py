

from doctest import testmod
from doctest import run_docstring_examples
from math import sqrt
phi = 1/2 + sqrt(5)/2
def improve_test():
        approx_phi = improve(golden_update, square_close_to_successor)
        assert approx_eq(phi, approx_phi), 'phi differs from its approximation'

def sum_naturals(n):
        """Return the sum of the first n natural numbers.

        >>> sum_naturals(10)
        55
        >>> sum_naturals(100)
        5051
        """
        total, k = 0, 1
        while k <= n:
            total, k = total + k, k + 1
        return total


def improve(update, close, guess=1):
        while not close(guess):
            guess = update(guess)
        return guess

def golden_update(guess):
        return 1/guess + 1

def square_close_to_successor(guess):
        return approx_eq(guess * guess, guess + 1)

def approx_eq(x, y, tolerance=1e-15):
        return abs(x - y) < tolerance
def average(x, y):
        return (x + y)/2
def sqrt(a):
        def sqrt_update(x):
            return average(x, a/x)
        def sqrt_close(x):
            return approx_eq(x * x, a)
        return improve(sqrt_update, sqrt_close)


print(sqrt(2))

def newton_update(f, df):
        def update(x):
            return x - f(x) / df(x)
        return update

def find_zero(f, df):
        def near_zero(x):
            return approx_eq(f(x), 0)
        return improve(newton_update(f, df), near_zero)
def square_root_newton(a):
        def f(x):
            return x * x - a
        def df(x):
            return 2 * x
        return find_zero(f, df)


def power(x, n):
        """Return x * x * x * ... * x for x repeated n times."""
        product, k = 1, 0
        while k < n:
            product, k = product * x, k + 1
        return product


def nth_root_of_a(n, a):
        def f(x):
            return power(x, n) - a
        def df(x):
            return n * power(x, n-1)
        return find_zero(f, df)

def curried_pow(x):
        def h(y):
            return pow(x, y)
        return h
def map_to_range(start, end, f):
        while start < end:
            print(f(start))
            start = start + 1




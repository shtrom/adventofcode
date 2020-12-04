#!/usr/bin/env python3
# usage:
#   ./day1.py < day1.in

import sys
import itertools

data = [int(n) for n in sys.stdin.readlines()]

# # Part 1
# pairs = itertools.combinations(data, 2)
# matching_pairs = {(a, b): a * b
#                   for a, b in pairs
#                   if a + b == 2020}
# print(matching_pairs)

# # Part 2
# pairs = itertools.combinations(data, 3)
# matching_pairs = {(a, b, c): a * b * c
#                   for a, b, c in pairs
#                   if a + b + c == 2020}
# print(matching_pairs)


# Generic solution
def day1(nentries=2):
    tuples = itertools.combinations(data, nentries)
    matching_tuples = {t: list(itertools.accumulate(t,
                                                    lambda a, b: a * b))[-1]
                       for t in tuples
                       if sum(t) == 2020}
    return matching_tuples


print(day1())
print(day1(3))

#!/usr/bin/env python3
# usage:
#   ./day2.py < day2.in

import sys
import re

input = sys.stdin.readlines()

splits = [re.split('[- :]+', s) for s in input]
passwords = [
    {'min': int(s[0]),
     'max': int(s[1]),
     'letter': s[2],
     'password': s[3],
     'len': len(re.findall(s[2], s[3]))
     }
    for s in splits]

# Part I
valid = [p for p in passwords
         if p['len'] >= p['min'] and p['len'] <= p['max']]
print(f'Part 1: {len(valid)}')

# Part II
valid = [p for p in passwords
         if (p['password'][p['min'] - 1] == p['letter']) ^ (p['password'][p['max'] - 1] == p['letter'])]
print(f'Part 2: {len(valid)}')

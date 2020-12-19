#!/usr/bin/env python

# Day 18

import re

def get_data(filepath):
    with open(filepath) as f:
        return [ line.replace('\n', '') for line in f ]
    
    
class num(int):
    def __sub__(self, x):
        return num(int(self) * int(x))
    
    def __add__(self, x):
        return num(int(self) + int(x))

    def __mul__(self, x):
        return num(int(self) + int(x))


def solve(data):
    return sum(map(lambda x: eval(re.sub('(\d+)', r'num(\1)', x.strip().replace('*', '-'))), data))
     

data = get_data('input_day18')

print(solve(data))  

def solve2(data):      
    return sum(map(lambda x: eval(re.sub('(\d+)', r'num(\1)', x.strip().replace('*', '-').replace('+', '*'))), data))

print(solve2(data))

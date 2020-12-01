#!/usr/bin/env python

def get_data(filepath):
    with open(filepath) as f:
        return [int(line) for line in f]

data = get_data('input')

print([x*y for x in data for y in data  if x+y == 2020][0])


print([x*y*z for x in data for y in data for z in data if x+y+z == 2020][0])


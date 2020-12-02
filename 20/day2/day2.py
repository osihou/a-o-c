#!/usr/bin/env python

from collections import Counter

def get_data(filepath):
    with open(filepath) as f:
        return [line for line in f]

def valid(line):
    ln = line.replace('\n', '').replace('-', ' ').replace(':', '').split(" ")
    return(int(ln[0]) <= (Counter(ln[3]))[ln[2]] <= int(ln[1]))

print(sum(list(map(valid, get_data('input_day2')))))

def valid2(line):
    ln = line.replace('\n', '').replace('-', ' ').replace(':', '').split(" ")
    return((((ln[3])[int(ln[0]) - 1] == ln[2]) and ((ln[3])[int(ln[1]) - 1] != ln[2])) or (((ln[3])[int(ln[1]) - 1] == ln[2]) and ((ln[3])[int(ln[0]) - 1] != ln[2])))
  

print(sum(list(map(valid2, get_data('input_day2')))))

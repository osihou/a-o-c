#!/usr/bin/env python


# Day 10

def get_data(filepath):
    with open(filepath) as f:
        return [int(line.replace('\n', '')) for line in f]
    
def count(lst):
        ones =  sum([ True for i in lst if i == 1])
        thries = sum([ True for i in lst if i == 3])
        return ones * thries
    
def chain(data):
    left = sorted([0] + data[:])
    right = sorted(data[:] + [max(data)+3]) 
    return [ right[i] - left[i]  for i in range(0, len(left))]

def arrangements(data):
    data.sort()
    data = [0] + data + [max(data)+3]
    links = {}
    count = {}
    for i in data:
        links[i] = [x for x in data if (x - i) > 0 and (x - i) <= 3]
    for i in data[::-1]:
        count[i] = sum([count[x] for x in links[i]]) if len(links[i]) > 0 else 1
    return(count[0])
        
        

data = get_data('input_day10')

# 1
print(count(chain(data)))

# 2
print(arrangements(data))

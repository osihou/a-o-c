#!/usr/bin/env python

# Day 9

def get_data(filepath):
    with open(filepath) as f:
        return [int(line.replace('\n', '')) for line in f]
    
def first_wrong(data,pnum):
    index = 0
    for i in data[pnum+index:]:
        preamble = data[index:pnum+index]
        index += 1        
        if i not in set([x + y for x in preamble for y in preamble if x != y]):
            return i

        
def weakness(data, pnum):
    fw = first_wrong(data,pnum)
    return [min(data[x:y]) + max(data[x:y]) for x in range(0, len(data)) for y in range(1, len(data)) if ((x < y) and (sum(data[x:y]) == fw)) ][0]
    
    
data = get_data('input_day9')

# 1
print(first_wrong(data,25))

# 2
print(weakness(data,25))

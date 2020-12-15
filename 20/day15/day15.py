#!/usr/bin/env python

# Day 15

def get_data(filepath):
    with open(filepath) as f:
        return [ (i+1, int(num)) for i, num in enumerate([line.replace('\n', '').split(',') for line in f][0]) ]
    
def num2020(data, ind):
    for i in range(len(data) - 1, ind - 1 ):
        k1,v1 = data[i]
        for k2, v2 in data[::-1][1:]:

            if v1 == v2:
                data += [(i+2, k1 - k2)]
                break
            elif k2 == 1 and v1 != v2:
                data += [(i+2, 0)]
                break
    return data[::-1][0][1]

data = get_data('input_day15')

# 1 
print(num2020(data, 2020))

def mapize(data):
    dt = {}
    for i,j in enumerate(data):
        dt[i+1] = int(j)
    return dt


def get_data30000000(filepath):
    with open(filepath) as f:
        return mapize([i for i in [line.replace('\n', '').split(',') for line in f]][0])


def num30000000(data, ind):
    for i in range(len(data), ind ):
        for j in range(0 , i):            
            lnum = i - j -1
            if data[i] == data[lnum]:
                data[i+1] =  i - lnum
                break
            elif lnum == 1:
                data[i+1] =  0
                break
        print(i)
    return data[ind]

data = get_data30000000('input_day15')

# 2
print(num30000000(data,30000000))

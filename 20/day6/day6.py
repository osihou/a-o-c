#!/usr/bin/env python

# Day 6

def get_data(filepath):
    with open(filepath) as f:
        return [line.replace('\n', '') for line in f]
    
def  refactor(data):
    
    def rep(line):
        if len(line) == 0:
            line = '###'
        return line
    
    def splt(line):
        return line.split('###')
    
    def spltid(line):
        return line.split(' ')
    
    def fltr(line):
        return list(filter(None, line))
    
    line = splt([' '.join(list(map(rep, data)))] [0])
    line = list(map(spltid, line))   
    line = list(map(fltr, line))  
    return line 
    
def count(data):
    lst = []
    for line in data:
        w = ''
        for word in line:
            w += word
        
        lst.append(len(set(list(w))))
        
    return lst


def count2(data):
    lst = []
    for line in data:
        w = []
        for word in line:
            w.append(set(list(word)))
        
        lst.append(len(w[0].intersection(*w)))
        
    return lst


data = refactor(get_data('input_day6'))

print(sum(count(data)))
print(sum(count2(data)))

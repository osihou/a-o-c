#!/usr/bin/env python

## Day 3

def get_data(filepath):
    with open(filepath) as f:
        return [line for line in f]

def lineup (data, slopey, slopex):
    num = - slopey
    colnum = len((data)[1].replace('\n', ''))
    
    item = lambda line, num : line.replace('\n', '')[num % (colnum)]
    result = []
    
    for line in data[::slopex]:
        num += slopey
        result.append(item(line, num) == '#')
        
    result.pop(0)
    return(result)

#1
print(sum(lineup(get_data('input_day3') , 3, 1)))   

#2
def slopestat(data):
    def root(slopey, slopex):
        return(sum(lineup(data , slopey, slopex)))
    return root

data = get_data('input_day3')
root = slopestat(data)
print(root(1,1) * root( 3, 1 ) * root( 7, 1 ) * root( 5, 1 ) * root( 1, 2 )) 

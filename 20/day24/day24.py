#!/usr/bin/env python

# Day 24

def get_data(filepath):
    with open(filepath) as f:
        return [ line.replace('\n', '') for line in f ]
    
def process_data(data):
    def rec_data(line):
        if len(line) == 0:
            return ''
        else:
            if line[0] in 's' or line[0] in 'n':
                return ' '+ line[0:2] +' '+ rec_data(line[2:]) + ''
            else:
                return ' ' + line[0] + ' '+ rec_data(line[1:]) + ''
            
    return list(map(lambda x : list(filter(None , rec_data(x).split(' '))), data))

def hex_color_pattern(data):  
    def move_hex(x):      
        if x == 'se':
            return (0, 1)
        elif x == 'sw':
            return (-1, 1)
        elif x == 'nw':
            return (0, -1)
        elif x == 'ne':
            return (1, -1)
        elif x == 'e':
            return (1, 0)
        elif x == 'w':
            return (-1, 0)
        
    def read_pattern(line):
        x = 0
        y = 0
        for n in line:
            dx, dy = move_hex(n)
            x += dx
            y += dy
        return str(x) + ' ' + str(y)
    
    
    dt = list(map(read_pattern, data))
    
    def dict_black(dtk):
        t = {}
        for i in dtk:
            if i not in t:
                t[i] = 'BLACK'
            else:
                if t[i] in 'BLACK':
                    t[i] = 'WHITE'
                else:
                    t[i] = 'BLACK'
        return t
    
    return dict_black(dt)
            
def count_black(dtk):
    return sum([True for x in list(dtk.values()) if x in 'BLACK'])


def cycle(itrn, pattern):
    
    def nei(x,y):
        return str(x)+' '+str(y)
    
    def neighbours(x, y):
        return [nei(x, y-1) , nei(x - 1, y), nei(x - 1, y + 1), nei(x + 1, y), nei(x, y + 1), nei(x + 1, y - 1)]
    
    for _ in range(itrn):
        def get_neighbours(dtk):
            dtkk = dtk.copy()
            for i in list(dtk):
                x, y = list(map(int, i.split(' ')))
                for j in neighbours(x,y):
                    if j not in dtk:
                        dtkk[j] = 'WHITE'                         
            return dtkk 
        
        dtk =  get_neighbours(pattern)
        dtkk = dtk.copy()
        
        for i in list(dtk):
            x, y = list(map(int, i.split(' ')))
            count = 0
            for j in neighbours(x,y):
                if j in dtk:
                    if dtk[j] in 'BLACK':
                        count += 1
                        
            if dtk[i] in 'BLACK':
                if count == 0 or count > 2:
                    dtkk[i] = 'WHITE'
            else:
                if count == 2:
                    dtkk[i] = 'BLACK'                
        pattern = dtkk.copy()        
    return(pattern)
                    
data = process_data(get_data('input_day24'))

pattern = hex_color_pattern(data)

# 1
print(count_black(pattern))

# 2
print(count_black(cycle(100, pattern)))

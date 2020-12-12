#!/usr/bin/env python

# Day 11

def get_data(filepath):
    with open(filepath) as f:
        return [line.replace('\n', '') for line in f]
    
def mutate(data):
    def top_bot ():
        ln = ''
        for i in range(0, len(data)+1):
            ln += '.'    
        return [ln] 
    def line_mutate(line):
        return '.' + line + '.'
    data = list(map(line_mutate, data))
    
    return top_bot() + data + top_bot()

def remutate(data):
    return list(map( lambda line : line[1:len(data)-1] , data[1:len(data)-1]))

def simulate(data):
    
    def count_occupied(data):
        return sum(map(lambda line: sum([True for x in line if x in '#']), data))   
    def rules(data): 
        
        def first_rule_check(ln, xn):
            return sum([True for x in list(map( lambda line : line[xn -1 : xn + 2] , data[ln -1 : ln + 2])) for i in x if i in '#' ]) - 1 >= 4
        def second_rule_check(ln,xn):
            return sum([True for x in list(map( lambda line : line[xn -1 : xn + 2] , data[ln -1 : ln + 2])) for i in x if i in '#' ]) == 0
        dt = data[:]
        ln = 0
        for line in data:
            xn = 0
            nln = ''
            for x in line:
                if (x in '#') and (first_rule_check(ln,xn)):
                    nln += 'L'
                elif (x in 'L') and (second_rule_check(ln,xn)):
                    nln += '#'
                else:
                    nln += x
                xn += 1
            dt[ln] = nln
            ln += 1     
        return dt
    
    num = -1   
    while True:
        nt = count_occupied(data)
        data = rules(data)
        if num == nt:
            return nt
        else:
            num = nt

# 1                           
simulate(mutate(get_data('input_day11')))

#!/usr/bin/env python

# Day 7

def get_data(filepath):
    with open(filepath) as f:
        return [line.replace('\n', '') for line in f]

def first_cls(data):   
    def count_bags(bag):
        rules = [line.split('s contain ')[0] for line in data if bag in line.split('contain')[1]]
        bags = set()
        for b in rules:
            bags.add(b)
            found = count_bags(b)
            if found:
                bags.update(found)               
        return bags    
    return len(count_bags('shiny gold bag'))
        
# 1      
print(first_cls(get_data('input_day7')))

def second_cls(data):
    def count_required(bag):
        rule = [line.split('s contain ')[1] for line in data if bag in line.split('contain')[0]]
        res = 0
        for b in rule[0].split(', '):
            n = b.split(' ')[0]
            if n.isnumeric(): 
                res += int(n) * (1 + count_required(b.split(' ')[1] + ' ' + b.split(' ')[2]))  
        return res
    return count_required('shiny gold bag')

# 2
print(second_cls(get_data('input_day7')))

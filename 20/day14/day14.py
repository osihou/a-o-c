#!/usr/bin/env python

# Day 14

def get_data(filepath):
    with open(filepath) as f:
        return [line.replace('\n', '').split(' = ') for line in f]


def sum_in_mem (data):
    zero_mask = '000000000000000000000000000000000000'
    def apply_mask(mask, value):
        value = zero_mask[:len(zero_mask) - len(value) ] + value
        new_value = ''
        for i in range(0, len(zero_mask)):
            if mask[i] not in 'X':
                new_value += mask[i]
            else:
                new_value += value[i]
        return int(new_value, 2)        
    mask = ''
    mem = {}
    for x in data:
        if x[0] in 'mask':
            mask = x[1]
        else:
            mem[x[0]] = apply_mask(mask, bin(int(x[1]))[2:])
            
    return sum(mem.values())
    
data = get_data('input_day14')

# 1
sum_in_mem(data)

from itertools import product
def sum_in_mem2 (data):
    zero_mask = '000000000000000000000000000000000000'
    def apply_mask(mask, value):
        value = zero_mask[:len(zero_mask) - len(value) ] + value
        new_value = ['0']*36
        for i in range(0, len(zero_mask)):
            if mask[i]  in '0':
                new_value[i] = value[i]
            elif mask[i] in 'X':
                new_value[i] = '{}'    
            else:
                new_value[i] = '1'
        return new_value       
    mask = ''
    mem = {}
    for x in data:
        if x[0] in 'mask':
            mask = x[1]
        else:
            addres = bin(int(x[0].replace('[', ' ').replace(']',' ').split(' ')[1]))[2:]
            value = bin(int(x[1]))[2:]
            resid = apply_mask(mask, addres)
            perms = [tuple(prod) for prod in product(('0', '1'), repeat = resid.count('{}'))]
            for perm in perms:
                new_addr = ''.join(resid).format(*perm)
                mem[int(new_addr, 2)] = int(value, 2)            
    return sum(mem.values())

# 2
sum_in_mem2(data)

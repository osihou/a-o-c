#!/usr/bin/env python

# Day 16

def get_data(filepath):
    with open(filepath) as f:
        return [ line.replace('\n', '') for line in f ]
    
def use_data(data):
    def repackage(data):
        dt = []
        row = []
        for line in data:     
            if len(line) == 0:
                dt.append(row[:]) 
                row = []
            else:
                row.append(line)
        dt.append(row[:])           
        return dt
    
    def rules(rl):
        fields = set()
        for r in rl:
            values = r.split(': ')[1].split(' or ')
            f = lambda x : range(x[0], x[1] + 1)
            fields = fields |  set(f ([int (x) for x in values[0].split('-')])) | set(f( [int (x) for x in values[1].split('-')]))
        return fields
    dt = repackage(data)
    rule = rules(dt[0])    
    def error_tickets(rl):
        errors = []
        rl = rl[1:]
        for r in rl: 
            values = set([int(i) for i in r.split(',')])
            errors += list(values - rule)
        return sum(errors)  
    return(error_tickets(dt[2]))
       
data = get_data('input_day16')

# 1
print(use_data(data))

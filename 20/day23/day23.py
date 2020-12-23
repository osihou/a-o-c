#!/usr/bin/env python


# Day 23

from iteration_utilities import deepflatten
import functools

def get_data(filepath):
    with open(filepath) as f:
        return [ line.replace('\n', '') for line in f ]

def dicircle(nums):
    srt = sorted(nums)
    pointer = (nums[0]) 
    
    def to_dt(nums):
        dt = {}
        for i,t in enumerate(nums):
            dt[t] = nums[(i+1)%len(nums)]
        return dt
    
    def to_lst(dtk, ptr, org_ptr):
        lst = list()
        if org_ptr != ptr:
            lst.append(ptr)
            lst.append(to_lst(dtk, dtk[ptr], org_ptr)) 
            return list(deepflatten(lst))
        else:
            return org_ptr
           
    def cycle(dtk, ptr):

        first = dtk[ptr]
        second = dtk[first]
        third = dtk[second]
        fourth = dtk[third]
          
        lt = [x for x in srt if ((x != first) and (x != second) and (x!= third)) ]

        ld = [i for i in lt if i < ptr or i == max(lt)]
        
        if len(ld) == 1:
            start = ld[0]
        else:
            start = ld[-2]
        
        end = dtk[start]
       
        dtk[ptr] = fourth
        dtk[start] = first
        dtk[third] = end
        
        return (dtk , to_lst(dtk, dtk[ptr], ptr)[0])
        
        
    def cycles(nr, ptr, dtk):
        for i in range(0, nr):
            dtk, ptr = cycle(dtk, ptr)     
        return functools.reduce(lambda a,b: a+b, [str(i) for i in to_lst(dtk, dtk[1] ,1) if i != 1])
        
    return cycles(100, pointer, to_dt(nums))

num = [int(x) for x in get_data('input_day23')[0]]

# 1
print(dicircle(num))

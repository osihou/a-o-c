#!/usr/bin/env python

## Day 4

def get_data(filepath):
    with open(filepath) as f:
        return [line for line in f]
    
def  refactor(data):
    
    def rep(line):
        line = line.replace('\n', '').replace(':', ' ')
        if len(line) == 0:
            line = '###'
        return(line)
    
    def splt(line):
        return(line.split('###'))
    
    def spltid(line):
        return(line.split(' '))
    
    def fltr(line):
        return(list(filter(None, line)))
    
    
    line = splt([' '.join(list(map(rep, data)))] [0])
    line = list(map(spltid, line))   
    line = list(map(fltr, line))  
    return(line)


#     byr (Birth Year)
#     iyr (Issue Year)
#     eyr (Expiration Year)
#     hgt (Height)
#     hcl (Hair Color)
#     ecl (Eye Color)
#     pid (Passport ID)
#     cid (Country ID)

def valid(line):
    return(all(x in line for x in match))

def valid_return(line):
    if all(x in line for x in match):
        return(line)
    

data = refactor( get_data('input_day4'))

# 1
print(sum(list(map(valid, data))))

res = list(filter(None, list(map(valid_return, data))))

class Passport:
    
    def __init__(self, raw):
        self.raw = raw
        

    def byr(value):
        return(1919 < int(value) < 2003)
    
    def iyr(value):
        return(2009 < int(value) < 2021)
    
    def eyr(value):
        return(2019 < int(value) < 2031)
    
    def hgt(value):
        if value[-2:] == 'cm':
            return value[:-2].isdigit() and 149 < int(value[:-2]) < 194
        elif value[-2:] == 'in':
            return value[:-2].isdigit() and 58 < int(value[:-2]) < 77
        else:
            return False
              
    def hcl(value):
        return(True if re.search(r"^#([a-fA-F0-9]{6})$", value) else False )
    
    def ecl(value):
        ec = ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth']
        return(value in ec)
    
    def pid(value):
        return(value.isnumeric() and (len(value) == 9))
    
    def cid(value):
        return(False)
    
    mpg = {
        'byr' : byr,
        'iyr' : iyr,
        'eyr' : eyr,
        'hgt' : hgt,
        'hcl' : hcl,
        'ecl' : ecl,
        'pid' : pid,
        'cid' : cid,
    }
     
    def process(self):
        types = self.raw[::2]
        values = self.raw[1::2]     
        dt = dict(zip(types, values))
        
        return(sum([(self.mpg.get(t))(dt[t]) for t in dt]) == 7)
        
        
def check_passports(data):
    return(Passport(data).process())
            
# 2            
print(sum(list(map(check_passports, res))))   

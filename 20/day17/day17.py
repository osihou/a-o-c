#!/usr/bin/env python

# Day 17

def get_data(filepath):
    with open(filepath) as f:
        return [ line.replace('\n', '') for line in f ]
    
def get_cord(x,y,z):
    return str(x - 1) + ' ' +str(1 - y) + ' ' + str(z)
    
def get_map(data):
    z = 0
    dt = {}
    for num,line in enumerate(data):
        for var, ch in enumerate(line):
            dt[get_cord(var,num,z)] = ch
    return dt

def adj1(dt, x, y, z):
    cnt = 0
    offsets = [-1, 0, 1]
    for dx in offsets:
        for dy in offsets:
            for dz in offsets:
                if dx == dy == dz == 0:
                    continue
                if get_cord(x + dx, y + dy, z + dz) in dt:
                    cube = dt[get_cord(x + dx, y + dy, z + dz)]
                else:
                    cube ='.'
                    
                if cube == "#":
                    cnt += 1
    return cnt

def evolve(data):
    dt = get_map(data)
    sizeX = len(data)
    sizeY = len(data)
    sizeZ = 0
    for i in range(0,6):
        dt_copy = dt.copy()
        sizeX  += 1
        sizeY  += 1
        sizeZ  += 1      
        for z in range(-sizeZ, sizeZ + 1):
            for y in range(-sizeY, sizeY + 1):
                for x in range(-sizeX, sizeX + 1):
                    
                    cnt = adj1(dt_copy, x, y, z)
                    
                    if get_cord(x, y, z) in dt_copy:
                        cube = dt_copy[get_cord(x, y, z)]
                    else:
                        cube = '.'
                        
                    if cube == "#" and cnt not in [2, 3]:
                        dt[get_cord(x, y, z)] = "."
                    elif cube == '.' and cnt == 3:
                        dt[get_cord(x, y, z)] = "#"
                        
    return sum([True for i in dt.values() if i in '#'])

def get_cord2(x,y,z,w):
    return str(x - 1) + ' ' +str(1 - y) + ' ' + str(z) + str(w)
    
def get_map2(data):
    z = 0
    w = 0
    dt = {}
    for num,line in enumerate(data):
        for var, ch in enumerate(line):
            dt[get_cord2(var,num,z,w)] = ch
    return dt

def adj2(dt, x, y, z, w):
    cnt = 0
    offsets = [-1, 0, 1]
    for dx in offsets:
        for dy in offsets:
            for dz in offsets:
                for dw in offsets:
                    if dx == dy == dz == dw == 0:
                        continue
                    if get_cord2(x + dx, y + dy, z + dz, w + dw) in dt:
                        cube = dt[get_cord2(x + dx, y + dy, z + dz, w + dw)]
                    else:
                        cube ='.'

                    if cube == "#":
                        cnt += 1
    return cnt

def evolve2(data):
    dt = get_map2(data)
    sizeX = len(data)
    sizeY = len(data)
    sizeZ = 0
    sizeW = 0
    for i in range(0,6):
        dt_copy = dt.copy()
        sizeX  += 1
        sizeY  += 1
        sizeZ  += 1
        sizeW  += 1
        for w in range(-sizeW, sizeW + 1):
            for z in range(-sizeZ, sizeZ + 1):
                for y in range(-sizeY, sizeY + 1):
                    for x in range(-sizeX, sizeX + 1):

                        cnt = adj2(dt_copy, x, y, z,w)

                        if get_cord2(x, y, z, w) in dt_copy:
                            cube = dt_copy[get_cord2(x, y, z, w)]
                        else:
                            cube = '.'

                        if cube == "#" and cnt not in [2, 3]:
                            dt[get_cord2(x, y, z, w)] = "."
                        elif cube == '.' and cnt == 3:
                            dt[get_cord2(x, y, z, w)] = "#"
                        
    return sum([True for i in dt.values() if i in '#'])

                
        
data = get_data('input_day17')    
            
    
print(evolve(data))

print(evolve2(data))


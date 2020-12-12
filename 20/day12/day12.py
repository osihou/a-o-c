#!/usr/bin/env python

# Day 12

def get_data(filepath):
    with open(filepath) as f:
        return [line.replace('\n', '') for line in f]


class Ship:
        
    def facing(self):
        p = self.position % 360
        if p == 0:
            return 'E'
        elif p == 90:
            return 'N'
        elif p == 180:
            return 'W'
        else:
            return 'S'
    
    def R(self,value):
        self.position -= int(value)

    def L(self,value):
        self.position += int(value)

    def N(self,value):
        self.north += int(value)

    def S(self,value):
        self.north -= int(value)

    def E(self,value):
        self.east += int(value)

    def W(self,value):
        self.east -= int(value)

    def F(self, value):
        getattr(self, self.facing())(int(value))
    
         
    def __init__(self, position, east, north):
        self.position = position
        self.east = east
        self.north = north
    
    def move(self, task):
       
            
        getattr(self, task[0])(task[1:])
               
    def print_postion(self):
        print([self.position, self.east, self.north])
    
    def print_manhattan(self):
        print(abs(self.east) + abs(self.north))
        
def navigation(data):
    ship = Ship(0,0,0)
    for task in data:
        ship.move(task)   
    return ship.print_manhattan()
        
data = get_data('input_day12')

# 1
navigation(data)
    
import math

class Ship2:

    def R(self,value):
        angle = (math.degrees(math.atan2(self.waypoint.east, self.waypoint.north)) + value) % 360.0
        radius = math.sqrt((self.waypoint.east) ** 2 + (self.waypoint.north) ** 2)
        radians = math.radians(angle)
        self.waypoint.north = radius * math.cos(radians)
        self.waypoint.east = radius * math.sin(radians)

    def L(self,value):
        angle = (math.degrees(math.atan2(self.waypoint.east, self.waypoint.north)) - value) % 360.0
        radius = math.sqrt((self.waypoint.east) ** 2 + (self.waypoint.north) ** 2)
        radians = math.radians(angle)
        self.waypoint.north = radius * math.cos(radians)
        self.waypoint.east = radius * math.sin(radians)

    def N(self,value):
        self.waypoint.north += value

    def S(self,value):
        self.waypoint.north -= value

    def E(self,value):
        self.waypoint.east += value

    def W(self,value):
        self.waypoint.east -= value

    def F(self, value):
        self.east += value * self.waypoint.east
        self.north += value * self.waypoint.north
          
    def __init__(self):
        self.waypoint = Ship(0, 10, 1)
        self.east = 0
        self.north = 0
    
    def move(self, task):  
        getattr(self, task[0])(int(task[1:]))
               
    def print_postion(self):
        print([self.east, self.north])
    
    def print_manhattan(self):
        print(abs(self.east) + abs(self.north))
        
def navigation2(data):
    ship = Ship2()
    for task in data:
        ship.move(task)   
    ship.print_manhattan()
 
# 2      
navigation2(data)


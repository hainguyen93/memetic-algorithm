
from __future__ import division
import getopt
from math import ceil
from math import log
from math import sqrt
import numpy as np
import operator
import random
import sys

# definition of hurdle problem
def hurdle(w, x):
    z = np.sum(1-x)
    return -ceil(z / w)-(z % w) / w
    
#flip a bit with probability p
def flip(b, p):
    r = random.random()
    if r <= p: 
        return 1-b
    else: 
        return b
        
# mutation operator with mutation rate p
def mutate(x, p):
    for i in range(x.size):
        x[i] = flip(x[i], p)
    return x
    
# sample the initial individual u.a.r.
def initialise(n):
    return np.array([random.randint(0, 1) for _ in range(n)])
    
# deep copy of array x to new array y
def clone(x):
    return np.array([i for i in x]);
    
def equal(x, y):
    epsilon = 0.0000001
    return abs(x-y) < epsilon
    
# first improvement local search
def fils(x, depth, w, t):
    indx = np.arange(x.size)
    for _ in xrange(depth):
        flag = False
        np.random.shuffle(indx)
        for i in indx:
            y = clone(x)
            y[i] = 1 - y[i] 
            t = t + 1
            if hurdle(w, y) > hurdle(w, x): 
                x = clone(y)
                flag = True
                break
        if not flag: 
            break
    return x, t
    
# best improvement local search
def bils(x, depth, w, t):
    for _ in xrange(depth):
        best_inds = []
        fittest  = hurdle(w, x)
        for i in xrange(x.size):
            y = clone(x)
            y[i] = 1- y[i]
            t = t + 1
            y_hurdle = hurdle(w, y)   
            if equal(y_hurdle, fittest):
                best_inds.append(y)
            elif y_hurdle > fittest:
                best_inds = [y]
                fittest = y_hurdle
        if len(best_inds) == 0:
            break
        else:
            r = random.randint(0, len(best_inds)-1)
            x = clone(best_inds[r])
    return x, t
    
# (1+1) EA
def one_one_ea(n, rate, w):
    p = rate / n
    t = 0
    x = initialise(n)
    while np.sum(x) != n:
        y = clone(x)
        y = mutate(y, p)
        t = t + 1
        if hurdle(w, y) >= hurdle(w, x):
            x = clone(y)
    return t
    
def two_one_ma(n, rate, depth, w, first):
    p = rate / n
    t = 0
    x1 = initialise(n)
    x2 = initialise(n)
    while ((np.sum(x1) != n) and (np.sum(x2) != n)):
        
        ## select two parents
        x1_hurdle = hurdle(w, x1)
        x2_hurdle = hurdle(w, x2)
        if equal(x1_hurdle, x2_hurdle):
            r1 = random.random()
            r2 = random.random()
            y1 = clone(x1) if r1 <= 0.5 else clone(x2)
            y2 = clone(x1) if r2 <= 0.5 else clone(x2)
        elif (x1_hurdle > x2_hurdle):
            y1 = clone(x1)
            y2 = clone(x1)
        else:
            y1 = clone(x2)
            y2 = clone(x2)
        
        ## crossover
        y = np.array([y1[i] if random.random() <= 0.5 else y2[i] for i in xrange(n)])
        
        ## mutation
        y = mutate(y, p) 
        
        ## call local search on offspring
        if first:
            y, t = fils(y, depth, w, t)
        else:
            y, t = bils(y, depth, w, t)
        
        y_hurdle = hurdle(w, y)
        tuples = {1:x1_hurdle, 2:x2_hurdle, 3:y_hurdle}
        sorted_tuples = sorted(tuples.items(), key=operator.itemgetter(1))
        
        ## update population
        first_place = sorted_tuples[2][0]
        second_place = sorted_tuples[1][0]
        if first_place == 1: x1 = clone(x1)
        elif first_place == 2: x1 = clone(x2)
        else: x1 = clone(y)
        
        if second_place == 1: x2 = clone(x1)
        elif second_place == 2: x2 = clone(x2)
        else: x2 = clone(y)
    return t
    
def main():  
    n = 100
    rate = 1
    #w = 10
    #print(str(two_one_ma(n, rate, w + 1, w, True)))
    for w in range(2, 11, 1):
        for id in xrange(100):
            print('' + str(w) + ' ' + str(id) + ' ' + str(two_one_ma(n, rate, w + 1, w, False)) + '\n')
    
     
if __name__ == "__main__":
    main()
    

    

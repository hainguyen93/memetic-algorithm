
from __future__ import division
import getopt
from math import ceil
from math import log
from math import sqrt
import numpy as np
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
    
def one_one_ma(n, rate, depth, w, first):
    p = rate / n
    t = 0
    x = initialise(n)
    while np.sum(x) != n:
        y = clone(x)
        y = mutate(y, p)     
        if first:
            y, t = fils(y, depth, w, t)
        else:
            y, t = bils(y, depth, w, t)
        if hurdle(w, y) >= hurdle(w, x):
            x = clone(y)
    return t
    
def main():  
    n = 100
    rate = 1
    for w in range(2, 11, 1):
        for id in xrange(100):
            print('' + str(w) + ' ' + str(id) + ' ' + str(one_one_ma(n, rate, w + 1, w, False)) + '\n')
    
     
if __name__ == "__main__":
    main()
    

    

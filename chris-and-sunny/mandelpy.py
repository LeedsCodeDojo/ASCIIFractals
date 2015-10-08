# -*- coding: utf-8 -*-
"""
Created on Wed Oct  7 19:04:30 2015

@author: christopherh
"""
import cmath

# function to decide whether complex number c is in the mandelbrot set
def isInTheSet(z,c,iterations, maxIterations=100, magnitudeLimit=2):
    if iterations > maxIterations:
        return -1
    elif abs(z) > magnitudeLimit:
        return iterations
    else:
        return isInTheSet(z**2+c, c, iterations+1, maxIterations, magnitudeLimit)

# functions to return different characters depending on number of iterations:
def justStar(x):
    return "*"

def asciiOrder(x):
    if x+40>126:
        return chr(126)
    else:
        return chr(x+40)

def asciiGreyScale(x):
    scale = " .,-~:=+*&#%$@"
    if x>=len(scale):
        return "@"
    else:
        return scale[x]


# rotate a complex number around the origin by a given angle in rads
def rotate(complexNumber, angle):
    polared_complex=cmath.polar(complexNumber)
    new_angle=angle+polared_complex[1]
    return cmath.rect(polared_complex[0],new_angle)


# generate the string representing the sampled points on the complex plane
# through a viewport defined by width, height, angle, xOrigin and yOrigin.
# xmax and ymax set the width and height of the output in characters 
def generateManlebrotString(width, height, xOrigin, yOrigin, angle=0, xmax=80, ymax= 25, func=asciiGreyScale):
    output = ""
            
    for j in range(ymax):
        for i in range(xmax):
            #choose complex sample value
            samplevalue = complex((i*width/float(xmax))+xOrigin, (j*height/float(ymax))+yOrigin)
            # check number of iterations taken to diverge
            # (returns -1 if doesn't diverge)
            iters = isInTheSet(0, rotate(samplevalue,angle), 0)
            if iters!=-1:
                output = output + func(iters)
            else:
                output = output + " "
        output = output + "\n"
        
    return output
        

# example invocation to print the whole set:

# print(generateManlebrotString(4,3,-2.4,-1.52,xmax=80,ymax=25,angle=0,func=asciiGreyScale))      
        

#lang racket

(define (mandelbrot width height iterations)
  (define (scale number x y)
    ((let* 
         ((xMinScale -2.5)
          (xMaxScale 1.0)
          (yMinScale -1.0)
          (yMaxScale 1.0)
          (xSourcePosition (/ number width))
          (ySourcePosition (/ number height))
          (realNumber (+ (* x (- xMaxScale xMinScale)) xMinScale))
          (imaginaryNumber (+ (* y (- yMaxScale yMinScale)) yMinScale))
          )
       (make-rectangular realNumber imaginaryNumber)
       ))
    )
  
  (define (inSet x y)
    "*"
    )
  
  (define (loopX xIndex yIndex)
    (cond
      ((<= xIndex 0) "")
      (else 
       (string-append (loopX (- xIndex 1) yIndex)  (inSet xIndex yIndex))
       )))
  
  
  (define (loopY yIndex)
    (cond
      ((<= yIndex 0) "")
      (else 
       (string-append (loopY (- yIndex 1)) (loopX width yIndex) "\n")
       )))
  
  (loopY height)
)


(display 
 (mandelbrot 80 40 10)
 )

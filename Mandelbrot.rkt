#lang racket

(define (mandelbrot width height iterations)
  (define (scale x y)
    (let* 
         ((xMinScale -2.5)
          (xMaxScale 1.0)
          (yMinScale -1.0)
          (yMaxScale 1.0)
          (xSourcePosition (/ x width))
          (ySourcePosition (/ y height))
          (realNumber (+ (* xSourcePosition (- xMaxScale xMinScale)) xMinScale))
          (imaginaryNumber (+ (* ySourcePosition (- yMaxScale yMinScale)) yMinScale)))
       (make-rectangular realNumber imaginaryNumber)))
  
  (define (iterationsToChar iterations)
    (string-ref "1234567890" iterations)
    )
  
  (define (inSet x y)
    (define (countValues currentIteration currentValue initialValue)
      (cond 
        ((> (magnitude currentValue) 2) currentIteration)
        ((> currentIteration iterations) currentIteration)
        (else (countValues (+ 1 currentIteration) (+ (* currentValue currentValue) initialValue) initialValue))))
    (let ((iterationCount (countValues 0 (scale x y) (scale x y)))) 
      (cond
        ((>= iterationCount iterations) " " )
        (else (string (string-ref "-=+0&£$?@#" iterationCount))))))
  
  (define (loopX xIndex yIndex)
    (cond
      ((<= xIndex 0) "")
      (else 
       (string-append (loopX (- xIndex 1) yIndex)  (inSet xIndex yIndex)))))
  
  (define (loopY yIndex)
    (cond
      ((<= yIndex 0) "")
      (else 
       (string-append (loopY (- yIndex 1)) (loopX width yIndex) "\n"))))
  
  (loopY height))

(display (mandelbrot 120 40 10))

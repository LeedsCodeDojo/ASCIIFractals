#lang racket

(define (mandelbrot width height iterations)
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
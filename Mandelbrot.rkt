#lang racket

(define (mandelbrot width height iterations)
  
  (define (loopX index)
    (cond
      ((<= index 0) "")
      (else 
       (string-append (loopX (- index 1)) "*")
       )))
  
  
  (define (loopY index)
    (cond
      ((<= index 0) "")
      (else 
       (string-append (loopY (- index 1)) (loopX width) "\n")
       )))
  
  (loopY height)
)

(display 
 (mandelbrot 80 40 10)
 )
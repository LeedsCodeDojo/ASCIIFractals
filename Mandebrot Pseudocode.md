Mandebrot Pseudocode
====================

Pseudocode to draw the Mandelbrot set using the 'escape time' algorithm, as adapted from [Wikipedia](https://en.wikipedia.org/wiki/Mandelbrot_set#Computer_drawings).

#### Overview

The main gist is that, for each pixel, you repeatedly apply an operation to the x,y numbers.  If the result heads towards infinity, the x,y point is not withing the set.  If it does not, the point is in the set.  (So the numbers in the set are the black, beetle-shaped bit in the middle).

As the concept of 'heading towards infinity' is a bit tricky to encode, the way it's done here is to just do the operation a load of times (e.g. 100) and see if the number stays within a certain limit.  If, after 100 iterations, the number is still within the limit, you count it as being in the set.  If it exceeds the limit before then, it's not in the set.

#### Colouring

To make it prettier, you can use different colours/characters/etc for the pixels which aren't in the set depending on which iteration they got to before the number exceeded the limit.

#### Scaling

One tricky aspect is that you need to convert between your output resolution (e.g. 80x25 'pixels' for a console window) and the important area of the Mandelbrot scale, which is roughly between -2.5 and 1.0 on the X axis, and -1.0 1nd 1.0 on the Y Axis.

An example of this would be, for the pixel 40, 5:

40, on the source scale 0..80, becomes -0.75 on the target scale -2.5..1.0
5, on the source scale 0..25, becomes -0.6 on the target scale -1.0..1.0

#### Pseudocode

    For each pixel (xPixel, yPixel) on the screen, do:
    {
      xScaled = xPixel scaled to lie in the Mandelbrot X scale (-2.5, 1)
      yScaled = yPixel scaled to lie in the Mandelbrot Y scale (-1, 1)
      x = 0.0
      y = 0.0
      iteration = 0
      max_iteration = 100
      while ( x*x + y*y < 2*2  AND  iteration < max_iteration )
      {
        xNext = x*x - y*y + xScaled
        y = 2*x*y + yScaled
        x = xNext
        iteration = iteration + 1
      }
      colour = palette[iteration]
      plot(xPixel, yPixel, colour)
    }

    To scale a number from a source scale to a target scale given sourceScaleMax, targetScaleMin, targetScaleMax:
    {
        positionInSourceScale = number / sourceScaleMax
        scaledNumber = positionInSourceScale * (targetScaleMax - targetScaleMin) + targetScaleMin
    }

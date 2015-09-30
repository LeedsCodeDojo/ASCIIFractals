Mandebrot Pseudocode
====================

Pseudocode to draw the Mandelbrot set using the 'escape time' algorithm, as adapted from [Wikipedia](https://en.wikipedia.org/wiki/Mandelbrot_set#Computer_drawings).

The main gist is that, for each pixel, you repeatedly apply an operation to the x,y numbers.  If the result heads towards infinity, the x,y point is not withing the set.  If it does not, the point is in the set.  (So the numbers in the set are the black, beetle-shaped bit in the middle).

As the concept of 'heading towards infinity' is a bit tricky to encode, the way it's done here is to just do the operation a load of times (e.g. 100) and see if the number stays within a certain limit.  If, after 100 iterations, the number is still within the limit, you count it as being in the set.  If it exceeds the limit before then, it's not in the set.

To make it prettier, you can use different colours/characters/etc for the pixels which aren't in the set depending on which iteration they got to before the number exceeded the limit.



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
        xtemp = x*x - y*y + xScaled
        y = 2*x*y + yScaled
        x = xtemp
        iteration = iteration + 1
      }
      colour = palette[iteration]
      plot(xPixel, yPixel, colour)
    }

    To scale a number from a source range to a target range given sourceRangeMax, targetRangeMin, targetRangeMax:
    {
        positionInSourceRange = number / sourceRangeMax
        scaledNumber = positionInSourceRange * (targetRangeMax - targetRangeMin) + targetRangeMin
    }

Mandebrot Pseudocode
====================

Pseudocode to draw the Mandelbrot set using the 'escape time' algorithm, as adapted from [Wikipedia](https://en.wikipedia.org/wiki/Mandelbrot_set#Computer_drawings)

    For each pixel (xPixel, yPixel) on the screen, do:
    {
      xScaled = xPixel scaled to lie in the Mandelbrot X scale (-2.5, 1)
      yScaled = yPixel scaled to lie in the Mandelbrot Y scale (-1, 1)
      x = 0.0
      y = 0.0
      iteration = 0
      max_iteration = 1000
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

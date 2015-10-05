using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Mandelbrot4
{
    class Program
    {
        static void Main(string[] args)
        {

            // Width of the screen
            var targetWidth = 36;

            // Height of the screen
            var targetHeight = 20;

            var scale = 1;// 0.25;
            var viewPointX = 0;// 30;
            var viewPointY = 0;// 18;

            const int MAX_ITERATION = 100;

            for (int pY = viewPointY; pY < targetHeight + viewPointY; pY++)
            {
                //y0 = scaled y coordinate of pixel (scaled to lie in the Mandelbrot Y scale (-1, 1))
                var y0 = ((2.0 / (double)targetHeight * scale) * (pY)) - 1.0;

                var row = "";
                for (int pX = viewPointX; pX < targetWidth + viewPointX; pX++)
                {

                    //x0 = scaled x coordinate of pixel (scaled to lie in the Mandelbrot X scale (-2.5, 1))
                    var x0 = ((3.5 / (double)targetWidth * scale) * (pX)) - 2.5;

                    var x = 0.0;
                    var y = 0.0;
                    var iteration = 0;
                    while (((x * x) + (y * y)) < (2 * 2) && iteration < MAX_ITERATION)
                    {
                        var xtemp = (x * x) - (y * y) + x0;
                        y = (2 * x * y) + y0;
                        x = xtemp;
                        iteration = iteration + 1;
                    }

                    row += (iteration == MAX_ITERATION) ? " " : "@";

                }

                Console.WriteLine(row);
                
            };
            Console.ReadKey(false);
        }
        
    }

}

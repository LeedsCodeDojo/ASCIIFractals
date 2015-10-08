using System;
using System.Drawing;
using System.Drawing.Imaging;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Mandelbrot2
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();

            DrawFullImage();

            DrawZoomedBox();
        }

        private void DrawFullImage()
        {
            var viewPort = new Rectangle(0, 0, this.ClientSize.Width, this.ClientSize.Height);
            var bmp = CreateBitmap(viewPort, this.ClientSize.Width, this.ClientSize.Height, 1);

            this.BackgroundImage = bmp;
        }

        private Bitmap CreateBitmap(Rectangle targetBitmapSize, int scaleWidth, int scaleHeight, int scale)
        {
            const int MAX_ITERATION = 100;


            Bitmap bmp = new Bitmap(targetBitmapSize.Width, targetBitmapSize.Height);
            BitmapData data = bmp.LockBits(new Rectangle(0, 0, bmp.Width, bmp.Height),
                                           ImageLockMode.ReadWrite, PixelFormat.Format24bppRgb);

            scaleWidth *= scale;
            scaleHeight *= scale;

            int stride = data.Stride;
            unsafe
            {
                byte* ptr = (byte*)data.Scan0;

                Parallel.For(0, targetBitmapSize.Width, pX =>
                 {
                     //x0 = scaled x coordinate of pixel (scaled to lie in the Mandelbrot X scale (-2.5, 1))
                     var x0 = ((3.5 / (double)scaleWidth) * (pX + targetBitmapSize.X)) - 2.5;

                     for (int pY = 0; pY < targetBitmapSize.Height; pY++)
                     {
                         //y0 = scaled y coordinate of pixel (scaled to lie in the Mandelbrot Y scale (-1, 1))
                         var y0 = ((2.0 / (double)scaleHeight) * (pY + targetBitmapSize.Y)) - 1.0;

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

                         if (iteration >= MAX_ITERATION)
                         {
                             ptr[(pX * 3) + pY * stride] = Color.Black.B;
                             ptr[(pX * 3) + pY * stride + 1] = Color.Black.G;
                             ptr[(pX * 3) + pY * stride + 2] = Color.Black.R;
                         }
                         else
                         {

                             //Normalise hue to be between 1 and 360
                             var hue = (360.0 / MAX_ITERATION) * iteration;
                             var col = ColorFromHSV(hue, 1, 1);

                             ptr[(pX * 3) + pY * stride] = (byte)col.B;
                             ptr[(pX * 3) + pY * stride + 1] = (byte)col.G;
                             ptr[(pX * 3) + pY * stride + 2] = (byte)col.R;
                         }
                     } //y
                 });

            }//unsafe
            bmp.UnlockBits(data);

            return bmp;
        } //fn

        public static Color ColorFromHSV(double hue, double saturation, double value)
        {
            int hi = Convert.ToInt32(Math.Floor(hue / 60)) % 6;
            double f = hue / 60 - Math.Floor(hue / 60);

            value = value * 255;
            int v = Convert.ToInt32(value);
            int p = Convert.ToInt32(value * (1 - saturation));
            int q = Convert.ToInt32(value * (1 - f * saturation));
            int t = Convert.ToInt32(value * (1 - (1 - f) * saturation));

            if (hi == 0)
                return Color.FromArgb(255, v, t, p);
            else if (hi == 1)
                return Color.FromArgb(255, q, v, p);
            else if (hi == 2)
                return Color.FromArgb(255, p, v, t);
            else if (hi == 3)
                return Color.FromArgb(255, p, q, v);
            else if (hi == 4)
                return Color.FromArgb(255, t, p, v);
            else
                return Color.FromArgb(255, v, p, q);
        }

        private void pictureBox1_MouseMove(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left)
            {
                pictureBox1.Left += (e.X - x);
                pictureBox1.Top += (e.Y - y);

                DrawZoomedBox();
            }
        }

        private void DrawZoomedBox()
        {
            var viewPort = new Rectangle(pictureBox1.Left * zoomLevel, pictureBox1.Top * zoomLevel, pictureBox1.Width, pictureBox1.Height);
            var bmp = CreateBitmap(viewPort, this.ClientSize.Width, this.ClientSize.Height, zoomLevel);

            this.pictureBox1.Image = bmp;
        }

        private void pictureBox1_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left)
            {
                x = e.X;
                y = e.Y;
            }
        }

        private int x;
        private int y;

        private int zoomLevel = 1;

        private void pictureBox1_DoubleClick(object sender, EventArgs e)
        {
            zoomLevel++;
            DrawZoomedBox();
        }


        //        total = 0
        //for (i = 0; i<max_iterations; i += 1)
        //{
        //  total += histogram[i]
        //    }

        //    hue = 0.0;
        //for (i = 0; i<iteration; i += 1)
        //{
        //  hue += histogram[i] / total // Must be floating-point division.
        //}

        //color = palette[hue]
    }
}

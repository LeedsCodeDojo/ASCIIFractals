#!/usr/bin/env ruby
# encoding: utf-8

require 'logger'
require 'paint'
require 'io/console'
require_relative 'rgb'

@log = Logger.new(STDOUT)
@log.level = Logger::INFO

@screen_y, @screen_x = $stdin.winsize.map {|s| s - 2}

@colors = RGB.colors.uniq.sample(200)
@character = "\u2592" ## .. or "#" works well too.

def mandlebrot
  (0..@screen_y).each do |y_pixel|
    (0..@screen_x). each do |x_pixel|
      x_scaled = scale(x_pixel, @screen_x.to_f, -2.5, 1.0)
      y_scaled = scale(y_pixel, @screen_y.to_f, -1.0, 1.0)
      @log.debug "#{x_pixel} scaled to #{x_scaled}"

      x = 0.0
      y = 0.0
      iteration = 0
      max_iteration = 100

      while (x * x + y*y < 2*2 && iteration < max_iteration) do
        x_next = x*x - y*y + x_scaled
        y = 2 * x * y + y_scaled
        x = x_next
        iteration = iteration + 1
      end
  
      hue = (360.0/ max_iteration) * iteration
      grey = (255.0/ max_iteration) * iteration
      @log.debug "hue: #{hue}"
      color = hsl_to_rgb(hue, 1, 1)
      @log.debug "color: #{color}"
      
      print Paint[@character, @colors[iteration]]
    end
    print "\n"
  end
end


def scale(number, source_max, target_min, target_max)
  in_src_scale = number / source_max
  @log.debug "in_src_scale: #{in_src_scale}"
  in_src_scale * (target_max - target_min) + target_min
end

# We tried to map a hue value into something that the 'paint'
# gem could print.  In the end we didn't have enough variation
# to be able to map to decent colours.  So, we loaded X11's
# rgb.txt, remove the duplicates and randomly pick 200 colours
# from the remaining set.
def hsl_to_rgb(h, s, l)
  @log.debug "hue: #{h}"

  q = l < 0.5 ? l * (1 + s) : l + s - l * s
  p = 2 * l - q
  r = hue_to_rgb(p, q, h + 1/3)
  g = hue_to_rgb(p, q, h)
  b = hue_to_rgb(p, q, h - 1/3)
  return [r*255, g*255, b*255]
  
end

def hue_to_rgb(p, q, t)

  t += 1 if t < 0
  t -= 1 if t > 1
  return p + (q - p) * 6 * t if t < 1/6
  return q if t < 1/2
  return p + (q - p) * (2/3 - t) * 6 if t < 2/3
  return p

end

mandlebrot()

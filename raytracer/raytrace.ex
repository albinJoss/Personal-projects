defmodule Raytrace do


  defmodule Vector do
    def smul({x1, x2, x3}, s) do
      {x1 * s, x2 * s, x3 * s}
    end

    def sub({x1, x2, x3}, {y1, y2, y3}) do
      {x1 - y1, x2 - y2, x3 - y3}
    end

    def add({x1, x2, x3}, {y1, y2, y3}) do
      {x1 + y1, x2 + y2, x3 + y3}
    end

    def norm({x1, x2, x3}) do
      x = (x1 * x1) + (x2 * x2) + (x3 * x3)
      :math.sqrt(x)
    end

    def dot({x1, x2, x3}, {y1, y2, y3}) do
      (x1 * y1) + (x2 * y2) + (x3 * y3)
    end

    def normalize(x) do
      {x1, x2, x3} = x
      {x1/norm({x1, x2, x3}), x2/norm({x1, x2, x3}), x3/norm({x1, x2, x3})}
    end
  end


  defmodule Ray do
    defstruct pos: {0, 0, 0}, dir: {0, 0, 1}
  end

  defprotocol Object do
    def intersect(object, ray)
    def normal(object, ray, pos)
  end

  defmodule Sphere do

    @color {1.0, 0.4, 0.4}
    @brilliance 0
    @transparency 0
    @refraction 1.5

    defstruct(
      radius: 2,
      pos: {0, 0, 0},
      color: @color,
      brilliance: @brilliance,
      transparency: @transparency,
      refraction: @refraction
    )


    defimpl Object do

      def intersect(sphere, ray) do
        Sphere.intersect(sphere, ray)
      end

      def normal(sphere, _, pos) do
        # assuming we always hit it from the outside
        Vector.normalize(Vector.sub(pos, sphere.pos))
      end

    end

    def intersect(%Sphere{pos: spos, radius: radius},  %Ray{pos: rpos, dir: dir}) do
      k = Vector.sub(spos, rpos)
      a = Vector.dot(dir, k)
      a2 = :math.pow(a, 2)
      k2 = :math.pow(Vector.norm(k), 2)
      r2 = :math.pow(radius, 2)
      t2 = a2 - k2 + r2
      closest(t2, a)
    end

    defp closest(t2, a) do
      if t2 < 0 do
        :no
      else
        t = :math.sqrt(t2)
        d1 = a - t
        d2 = a + t

        cond do
          d1 > 0.0 and d2 > 0.0 ->
            {:ok, min(d1, d2)}
          d1 > 0.0 ->
            {:ok, d1}
          d2 > 0.0 ->
            {:ok, d2}
          true ->
            :no
        end
      end
    end





  end

  defmodule Camera do
    defstruct pos: nil, corner: nil, right: nil, down: nil, size: nil


    def normal(size) do
      {width, height} = size
      d = width * 1.2
      h = width / 2
      v = height / 2
      pos = {0, 0, 0}
      corner = {-h, v, d}
      right = {1, 0, 0}
      down = {0, -1, 0}
      %Camera{pos: pos, corner: corner, right: right , down: down , size: size}
    end

    def ray(%Camera{pos: pos, corner: corner, right: right, down: down, size: _size}, x, y) do
      x_pos = Vector.smul(right, x)
      y_pos = Vector.smul(down, y)
      pixel = Vector.add(corner, Vector.add(x_pos, y_pos))
      dir = Vector.normalize(pixel)
      %Ray{pos: pos, dir: dir}
    end
  end

  defmodule World do
    @background {0, 0, 0}
    @depth 2
    @ambient {0.3, 0.3, 0.3}
    @refraction 1
    defstruct objects: [], lights: [], background: @background, depth: @depth, ambient: @ambient, refraction: @refraction
  end

  defmodule Light do
    defstruct pos: nil, color: {1.0, 1.0, 1.0}

    def illuminate(obj, ill, world) do
      color = obj.color
      ill(color, mul(ill, world.ambient))
    end

    def illuminate(obj, refl, refr, ill, world) do
      surface = ill(obj.color, mul(ill, world.ambient))
      mul(add(surface, refr, obj.transparency), mod(refl, obj.brilliance))
    end

    def illuminate(obj, refl, ill, world) do
      surface = ill(obj.color, mul(ill, world.ambient))
      mul(surface, mod(refl, obj.brilliance))
    end

    def combine(point, normal, lights) do
      List.foldl(lights, {0, 0, 0},
      fn(light, contr) ->
        next = contribute(point, normal, light.pos, light.color)
        mul(next, contr)
      end)
    end

    def contribute(point, normal, source, {r, g, b}) do
      direction = Vector.normalize(Vector.sub(source, point))
      cos = Vector.dot(direction, normal)
      if cos >= 0 do
        {r * cos, g * cos, b * cos}
      else
        {0, 0, 0}
      end
    end

    def mul({r1, b1, g1}, {r2, b2, g2}) do
      {1 - (1 - r1) * (1 - r2), 1 - (1 - g1) * (1 - g2), 1 - (1 - b1) * (1 - b2)}
    end

    def ill({r1, b1, g1}, {r2, b2, g2}) do
      {r1 * r2, g1 * g2, b1 * b2}
    end

    def mod({r1, g1, b1}, t) do
      {r1 * t, g1 * t, b1 * t}
    end

    def add({r1, b1, g1}, {r2, b2, g2}, t) do
      s = 1 - t
      {r1 * s + r2 * t, g1 * s + g2 * t, b1 * s + b2 * t}
    end

    def check({r, g, b}) do
      (r >= 0) and (r <= 255) and (g >= 0) and (g <= 255) and (b >= 0) and (b <= 255)
    end
  end

  defmodule Tracer do
    @delta 0.001

    def intersect(ray, objects) do
      List.foldl(objects, {:inf, :nil}, fn(object, sofar) ->
        {dist, _} = sofar

      case Object.intersect(object, ray) do
        {:ok, d} when d < dist ->
          {d, object}
        _ ->
          sofar
      end
    end)
  end

  def trace(x, y, camera, world) do
    ray = Camera.ray(camera, x, y)
    trace(ray, world.depth, world)
  end

  defp trace(_ray, 0, world) do
    world.background
  end

  ##def trace(ray, objects) do
   ## case intersect(ray, objects) do
    ##  {:inf, _} ->
    ##    @black
    ##  {_, object} ->
    ##  object.color
  ##end
  ##end

  defp trace(ray, depth, world) do

    case intersect(ray, world.objects) do
      {:inf, _} ->
        world.background

      {dist, obj} ->
        i = Vector.add(ray.pos, Vector.smul(ray.dir, dist - @delta))
        normal = Object.normal(obj, ray, i)
        visible = visible(i, world.lights, world.objects)
        illumation = Light.combine(i, normal, visible)
        r = %Ray{pos: i, dir: reflection(ray.dir, normal)}
        reflection = trace(r, depth - 1, world)
        Light.illuminate(obj, reflection, illumation, world)

    end
  end

  def reflection(l, n) do
    Vector.sub(l, Vector.smul(n, 2 * Vector.dot(l, n)))
  end

    def visible(point, lights, objs) do
      Enum.filter(lights, fn light ->
        clear(point, light.pos, objs)
      end)
    end

    def clear(point, orgin, objs) do
      dir = Vector.normalize(Vector.sub(orgin, point))

      List.foldl(objs, true, fn (obj, acc) ->
        case acc do
          false ->
            false

          true ->
            case Object.intersect(obj, %Ray{pos: point, dir: dir}) do
              :no ->
                true
              _ ->
                false
        end
      end
    end)
  end

  def tracer(camera, world) do
    {w, h} = camera.size
    for y <- 1..h, do: for(x <- 1..w, do: trace(x, y, camera, world))
  end
end

  defmodule Plane do
    @color {1.0, 0.4, 0.4}
    @brilliance 0
    @transparency 0
    @refraction

    defstruct(
      pos0: {0, 0, 0},
      pos1: {1, 0, 0},
      pos2: {0, 1, 0}
      color:
    )
  end
  defmodule Test do
    def snap do
    camera = Camera.normal({800, 600})

      obj1 = %Sphere{radius: 140, pos: {0, 0, 700}, color: {1, 0.5, 0}}
      obj2 = %Sphere{radius: 50, pos: {200, 0, 600}, color: {0, 0.8, 0.2}}
      obj3 = %Sphere{radius: 50, pos: {-80, 0, 400}, color: {0.1, 0.1, 1}}

      light1 = %Light{pos: {-1000, 1000, 700}, color: {1.0, 0.3, 0.3}}
      light2 = %Light{pos: {800, 800, 0}, color: {0.3, 1.0, 0.3}}
      light3 = %Light{pos: {800, -800, 0}, color: {0.3, 0.3, 1.0}}

      world = %World{
        objects: [obj1, obj2, obj3],
        lights: [light1, light2, light3],
        background: {0.0, 0.0, 0.0},
        ambient: {0.6, 0.6, 0.6}
      }
      image = Tracer.tracer(camera, world)
      PPM.write("snap2.ppm", image)

  end
end

defmodule PPM do

  # write(Name, Image) The image is a list of rows, each row a list of
  # tuples {R,G,B} where the values are flots from 0 to 1. The image
  # is written using PMM format P6 and color depth 0-255. This means that
  # each tuple is written as three bytes.

  def write(name, image) do
    height = length(image)
    width = length(List.first(image))
    {:ok, fd} = File.open(name, [:write, :binary])
    IO.puts(fd, "P6")
    IO.puts(fd, "#generated by ppm.ex")
    IO.puts(fd, "#{width} #{height}")
    IO.puts(fd, "255")
    rows(image, fd)
    File.close(fd)
  end

  defp rows(rows, fd) do
    Enum.each(rows, fn r ->
      colors = row(r)
      IO.write(fd, colors)
    end)
  end

  defp row(row) do
    List.foldr(row, [], fn({r, g, b}, a) ->
      [trunc(r * 255), trunc(g * 255), trunc(b * 255) | a]
    end)
  end

end
end

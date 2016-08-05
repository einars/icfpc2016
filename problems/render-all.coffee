#!/usr/bin/coffee

fs = require 'fs'
Canvas = require 'canvas'
sprintf = require 'sprintf'

w = 500
h = 500

draw_canvas = (ctx, spec, fit) ->

  p = parse spec

  ctx.fillStyle = '#eee'
  ctx.fillRect 0, 0, w, h
  ctx.fillStyle = undefined


  if fit
    # h × 0.1 — minor padding reserved
    scale = (h * 0.9) / (p.size)
    base_x = p.x_min - (0.05 * p.size)
    base_y = p.y_min - (0.05 * p.size)
  else
    # h × 0.2 — minor padding reserved
    scale = (h * 0.8)
    # center object
    base_x = p.x_min - (0.1 * p.size) - (1 - (p.x_max - p.x_min)) / 2
    base_y = p.y_min - (0.1 * p.size) - (1 - (p.y_max - p.y_min)) / 2


  ctx_moveto = (ctx, coords) ->
    x = (coords.x - base_x) * scale
    y = h - (coords.y - base_y) * scale
    ctx.moveTo x, y

  ctx_lineto = (ctx, coords) ->
    x = (coords.x - base_x) * scale
    y = h - (coords.y - base_y) * scale
    ctx.lineTo x, y


  ctx.setLineDash [5, 3]
  ctx.strokeStyle = '#555555'
  ctx.lineWidth = 0.7
  for line in p.skels
    ctx.beginPath()
    ctx_moveto ctx, line.p1
    ctx_lineto ctx, line.p2
    ctx.stroke()

  ctx.setLineDash []
  ctx.strokeStyle = '#339933'
  ctx.fillStyle = '#'
  ctx.lineWidth = 1.5
  for pts in p.polys
    ctx.beginPath()
    ctx_moveto ctx, pts[0]
    for i in [1...pts.length]
      ctx_lineto ctx, pts[i]
    ctx_lineto ctx, pts[0]
    ctx.stroke()



make_coord = (coord) ->

  coords = coord.match /[^\/]+/g
  if coords[1]
    return (1 * coords[0]) / (1 * coords[1])
  else
    return (1 * coords[0])


make_pt = (pt) ->

  xy = pt.match /[^,]+/g
  return {
    x: make_coord xy[0]
    y: make_coord xy[1]
  }

line_of_pts = (pt1, pt2) ->
  return {
    p1: pt1
    p2: pt2
    key: sprintf "%.4f %.4f %.4f %.4f", pt1.x, pt1.y, pt2.x, pt2.y
  }

make_line = (pts) ->
  ptpt = pts.match /[^ ]+/g
  return line_of_pts (make_pt ptpt[0]), (make_pt ptpt[1])

parse = (spec) ->
  lines = spec.match /[^\r\n]+/g
  return if not lines

  n_poly = 1 * lines[0]
  cur = 1
  polys = []
  all_pts = []
  x_min = undefined
  x_max = undefined
  y_min = undefined
  y_max = undefined
  for i in [1..n_poly]
    poly = []
    poly_len = lines[cur]
    cur += 1
    for j in [1..poly_len]
      pt = make_pt lines[cur]
      poly.push pt

      if x_min == undefined or x_min > pt.x
        x_min = pt.x
      if x_max == undefined or x_max < pt.x
        x_max = pt.x
      if y_min == undefined or y_min > pt.y
        y_min = pt.y
      if y_max == undefined or y_max < pt.y
        y_max = pt.y

      cur++
    polys.push poly

  skels = []
  n_skels = lines[cur]
  cur++

  for i in [1..n_skels]
    skels.push make_line lines[cur]
    cur++

  return {
    x_min: x_min
    x_max: x_max
    y_min: y_min
    y_max: y_max
    size: Math.max (y_max - y_min), (x_max - x_min)
    polys: polys
    skels: skels
  }


main = ->

  for fit in [true, false]
    for i in [1..101]
      ca = new Canvas w, h
      ctx = ca.getContext '2d'
      spec = fs.readFileSync (sprintf './%03d.txt', i),
        encoding: 'utf-8'
      draw_canvas ctx, spec, fit
      fs.writeFileSync (sprintf './%s-%03d.png', (if fit then 'fit' else 'nofit'), i), ca.toBuffer()

main()


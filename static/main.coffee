specs = {

  7: """
1
4
4328029871649615121465353437184/8656059743299229793415925725865,-1792728671193156318471947026432/8656059743299229793415925725865
10448788414492386111887872752297/8656059743299229793415925725865,4328029871649615121465353437184/8656059743299229793415925725865
4328029871649614671950572288681/8656059743299229793415925725865,10448788414492386111887872752297/8656059743299229793415925725865
-1792728671193156318471947026432/8656059743299229793415925725865,4328029871649614671950572288681/8656059743299229793415925725865
4
4328029871649615121465353437184/8656059743299229793415925725865,-1792728671193156318471947026432/8656059743299229793415925725865 10448788414492386111887872752297/8656059743299229793415925725865,4328029871649615121465353437184/8656059743299229793415925725865
4328029871649615121465353437184/8656059743299229793415925725865,-1792728671193156318471947026432/8656059743299229793415925725865 -1792728671193156318471947026432/8656059743299229793415925725865,4328029871649614671950572288681/8656059743299229793415925725865
-1792728671193156318471947026432/8656059743299229793415925725865,4328029871649614671950572288681/8656059743299229793415925725865 4328029871649614671950572288681/8656059743299229793415925725865,10448788414492386111887872752297/8656059743299229793415925725865
10448788414492386111887872752297/8656059743299229793415925725865,4328029871649615121465353437184/8656059743299229793415925725865 4328029871649614671950572288681/8656059743299229793415925725865,10448788414492386111887872752297/8656059743299229793415925725865
"""
  32: """
1
8
7/12,7/12
2/3,2/3
2/3,5/6
5/6,1
2/3,1
5/6,7/6
1/2,7/6
1/2,2/3
11
7/12,7/12 2/3,2/3
1/2,2/3 1/2,7/6
7/12,3/4 7/12,1
2/3,2/3 2/3,7/6
1/2,2/3 5/6,1
1/2,5/6 5/6,7/6
7/12,7/12 1/2,2/3
2/3,2/3 1/2,5/6
2/3,5/6 1/2,1
1/2,1 5/6,1
1/2,7/6 5/6,7/6

"""

}

angular.module('vis', ['sprintf'])
.controller 'VisController', ($scope, $timeout) ->

  $scope.source = specs[32]

  $scope.updateCanvas = ->
    $scope.dbg.length = 0
    log 'updateCanvas'
    p = parse $scope.source

    return if p == undefined

    log JSON.stringify p


    $cv = $('.js-canvas')
    w = $cv.width()
    h = $cv.height()
    ctx = $('.js-canvas').get(0).getContext '2d'
    $cv.get(0).width = w
    $cv.get(0).height = h
    ctx.width = w
    ctx.height = h

    scale = (h * 0.9) / (p.size)

    base_x = p.x_min - (0.05 * p.size)
    base_y = p.y_min - (0.05 * p.size)

    log 'scale ' + scale
    log 'base_x ' + base_x

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
    _.each p.skels, (line) ->
      ctx.beginPath()
      ctx_moveto ctx, line.p1
      ctx_lineto ctx, line.p2
      ctx.stroke()

    ctx.setLineDash []
    ctx.strokeStyle = '#339933'
    ctx.lineWidth = 1.5
    _.each p.polys, (pts) ->
      ctx.beginPath()
      ctx_moveto ctx, pts[0]
      for i in [1...pts.length]
        ctx_lineto ctx, pts[i]
      ctx_lineto ctx, pts[0]
      ctx.stroke()



  $scope.clearCanvas = ->
    $scope.source = ''
    $scope.dbg.length = 0
    $scope.updateCanvas()

    $timeout( -> $('.js-source').focus())

  $scope.dbg = []

  log = (t) ->
    if typeof t is 'string'
      $scope.dbg.push {text: t}
    else
      $scope.dbg.push {text: JSON.stringify t}

  log 'viscontroller started'


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
    log 'polygons: ' + n_poly
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
      log 'poly ' + i + ', pts: ' + poly_len
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

    log 'x min: ' + x_min
    log 'x max: ' + x_max
    log 'y min: ' + y_min
    log 'y max: ' + y_max

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




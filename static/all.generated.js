(function() {
  var specs;

  specs = {
    7: "1\n4\n4328029871649615121465353437184/8656059743299229793415925725865,-1792728671193156318471947026432/8656059743299229793415925725865\n10448788414492386111887872752297/8656059743299229793415925725865,4328029871649615121465353437184/8656059743299229793415925725865\n4328029871649614671950572288681/8656059743299229793415925725865,10448788414492386111887872752297/8656059743299229793415925725865\n-1792728671193156318471947026432/8656059743299229793415925725865,4328029871649614671950572288681/8656059743299229793415925725865\n4\n4328029871649615121465353437184/8656059743299229793415925725865,-1792728671193156318471947026432/8656059743299229793415925725865 10448788414492386111887872752297/8656059743299229793415925725865,4328029871649615121465353437184/8656059743299229793415925725865\n4328029871649615121465353437184/8656059743299229793415925725865,-1792728671193156318471947026432/8656059743299229793415925725865 -1792728671193156318471947026432/8656059743299229793415925725865,4328029871649614671950572288681/8656059743299229793415925725865\n-1792728671193156318471947026432/8656059743299229793415925725865,4328029871649614671950572288681/8656059743299229793415925725865 4328029871649614671950572288681/8656059743299229793415925725865,10448788414492386111887872752297/8656059743299229793415925725865\n10448788414492386111887872752297/8656059743299229793415925725865,4328029871649615121465353437184/8656059743299229793415925725865 4328029871649614671950572288681/8656059743299229793415925725865,10448788414492386111887872752297/8656059743299229793415925725865",
    32: "1\n8\n7/12,7/12\n2/3,2/3\n2/3,5/6\n5/6,1\n2/3,1\n5/6,7/6\n1/2,7/6\n1/2,2/3\n11\n7/12,7/12 2/3,2/3\n1/2,2/3 1/2,7/6\n7/12,3/4 7/12,1\n2/3,2/3 2/3,7/6\n1/2,2/3 5/6,1\n1/2,5/6 5/6,7/6\n7/12,7/12 1/2,2/3\n2/3,2/3 1/2,5/6\n2/3,5/6 1/2,1\n1/2,1 5/6,1\n1/2,7/6 5/6,7/6\n"
  };

  angular.module('vis', []).controller('VisController', function($scope, $timeout) {
    var log, make_coord, make_pt, parse;
    $scope.source = specs[32];
    $scope.updateCanvas = function() {
      var $cv, base_x, base_y, ctx, ctx_lineto, ctx_moveto, h, p, scale, w;
      $scope.dbg.length = 0;
      log('updateCanvas');
      p = parse($scope.source);
      if (p === void 0) {
        return;
      }
      log(JSON.stringify(p));
      $cv = $('.js-canvas');
      w = $cv.width();
      h = $cv.height();
      ctx = $('.js-canvas').get(0).getContext('2d');
      $cv.get(0).width = w;
      $cv.get(0).height = h;
      ctx.width = w;
      ctx.height = h;
      log('Canvas: ' + ctx.width + ' x ' + ctx.height);
      scale = (h * 0.9) / p.size;
      base_x = p.x_min - (0.05 * p.size);
      base_y = p.y_min - (0.05 * p.size);
      log('scale ' + scale);
      log('base_x ' + base_x);
      ctx_moveto = function(ctx, coords) {
        var x, y;
        log('moveto');
        log(JSON.stringify(coords));
        x = (coords.x - base_x) * scale;
        y = h - (coords.y - base_y) * scale;
        log(x + ' / ' + y);
        return ctx.moveTo(x, y);
      };
      ctx_lineto = function(ctx, coords) {
        var x, y;
        log('lineto');
        log(JSON.stringify(coords));
        x = (coords.x - base_x) * scale;
        y = h - (coords.y - base_y) * scale;
        log(x + ' / ' + y);
        return ctx.lineTo(x, y);
      };
      return _.each(p.polys, function(pts) {
        var i, k, ref;
        ctx.beginPath();
        ctx.strokeStyle = 'blue';
        ctx_moveto(ctx, pts[0]);
        for (i = k = 1, ref = pts.length; 1 <= ref ? k < ref : k > ref; i = 1 <= ref ? ++k : --k) {
          ctx_lineto(ctx, pts[i]);
        }
        ctx_lineto(ctx, pts[0]);
        return ctx.stroke();
      });
    };
    $scope.clearCanvas = function() {
      $scope.source = '';
      $scope.dbg.length = 0;
      $scope.updateCanvas();
      return $timeout(function() {
        return $('.js-source').focus();
      });
    };
    $scope.dbg = [];
    log = function(t) {
      if (typeof t === 'string') {
        return $scope.dbg.push({
          text: t
        });
      } else {
        return $scope.dbg.push({
          text: JSON.stringify(t)
        });
      }
    };
    log('viscontroller started');
    make_coord = function(coord) {
      var coords;
      coords = coord.match(/[^\/]+/g);
      if (coords[1]) {
        return (1 * coords[0]) / (1 * coords[1]);
      } else {
        return 1 * coords[0];
      }
    };
    make_pt = function(pt) {
      var xy;
      xy = pt.match(/[^,]+/g);
      return {
        x: make_coord(xy[0]),
        y: make_coord(xy[1])
      };
    };
    return parse = function(spec) {
      var all_pts, cur, i, j, k, l, lines, n_poly, poly, poly_len, polys, pt, ref, ref1, x_max, x_min, y_max, y_min;
      lines = spec.match(/[^\r\n]+/g);
      if (!lines) {
        return;
      }
      n_poly = 1 * lines[0];
      log('polygons: ' + n_poly);
      cur = 1;
      polys = [];
      all_pts = [];
      x_min = void 0;
      x_max = void 0;
      y_min = void 0;
      y_max = void 0;
      for (i = k = 1, ref = n_poly; 1 <= ref ? k <= ref : k >= ref; i = 1 <= ref ? ++k : --k) {
        poly = [];
        poly_len = lines[cur];
        cur += 1;
        log('poly ' + i + ', pts: ' + poly_len);
        for (j = l = 1, ref1 = poly_len; 1 <= ref1 ? l <= ref1 : l >= ref1; j = 1 <= ref1 ? ++l : --l) {
          pt = make_pt(lines[cur]);
          poly.push(pt);
          if (x_min === void 0 || x_min > pt.x) {
            x_min = pt.x;
          }
          if (x_max === void 0 || x_max < pt.x) {
            x_max = pt.x;
          }
          if (y_min === void 0 || y_min > pt.y) {
            y_min = pt.y;
          }
          if (y_max === void 0 || y_max < pt.y) {
            y_max = pt.y;
          }
          cur++;
        }
        polys.push(poly);
      }
      log('x min: ' + x_min);
      log('x max: ' + x_max);
      log('y min: ' + y_min);
      log('y max: ' + y_max);
      return {
        x_min: x_min,
        x_max: x_max,
        y_min: y_min,
        y_max: y_max,
        size: Math.max(y_max - y_min, x_max - x_min),
        polys: polys
      };
    };
  });

}).call(this);

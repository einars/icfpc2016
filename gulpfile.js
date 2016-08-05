var gulp = require('gulp');
var concat = require('gulp-concat');
var sass = require('gulp-sass');
var merge = require('merge-stream');
var plumber = require('gulp-plumber');
var coffee = require('gulp-coffee');
var bower = require('bower-files')();

gulp.task('scripts', function(){

  var s_js = gulp.src(bower.ext('js').files);
  s_js = gulp.src([]);

  var s_coffee = gulp.src([
    './static/main.coffee'
  ])
    .pipe(plumber())
    .pipe(coffee());

  return merge(s_js, s_coffee)
    .pipe(concat('all.generated.js'))
    .pipe(gulp.dest('./static'));
});


gulp.task('stylesheets', function() {

  var s_sass = gulp.src([
      './static/main.scss'
  ])
    .pipe(plumber())
    .on('error', console.log)
    .pipe(sass());

  return s_sass
    .pipe(concat('all.generated.css'))
    .pipe(gulp.dest('./static'));
});



gulp.task('watch', function(){
    gulp.watch([
      './static/**/*.scss',
    ], ['stylesheets']);
    gulp.watch([
      './static/**/*.coffee',
    ], ['scripts']);
});

gulp.task('default', ['stylesheets', 'scripts', 'watch']);


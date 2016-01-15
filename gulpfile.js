var gulp = require('gulp');
var elm = require('gulp-elm');
var rename = require('gulp-rename');

gulp.task('elm-init', elm.init);

function swallowError (error) {
  console.log(error.toString());
  this.emit('end');
}

gulp.task('elm', ['elm-init'], function () {
  return gulp.src('./Quiz.elm')
    .pipe(elm())
    .on('error', swallowError)
    .pipe(rename('quiz.js'))
    .pipe(gulp.dest('./'))
});

gulp.task('default', ['elm'], function () {
  gulp.watch('./*.elm', ['elm']);
});

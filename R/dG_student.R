dG_student <-
function(x_i, x, d, v) {
  drop(x_i/(1 + (crossprod(x))/v))
}

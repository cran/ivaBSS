d2G_student <-
function(x_i, x, d, v) {
  drop(1/(1 + crossprod(x)/v) - (2*x_i^2)/((1 + crossprod(x)/v)^2*v))
}

dG_laplace_diag <-
function(x_i, x, d,theta) {
  x_i/drop(sqrt(crossprod(x)))
}

d2G_laplace_diag <-
function(x_i, x, d, theta) {
  drop(crossprod(x))^(-1/2) - drop(crossprod(x))^(-3/2)*x_i^2
}

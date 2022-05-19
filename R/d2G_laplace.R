d2G_laplace <-
function(x_i, x, d, inv_Sigma) {
  drop(crossprod(x,inv_Sigma%*%x))^(-1/2)*inv_Sigma[d,d] - drop(crossprod(x, inv_Sigma%*%x))^(-3/2)*drop(x%*%(inv_Sigma[,d]))^2
}

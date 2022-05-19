dG_laplace <-
function(x_i, x, d, inv_Sigma) {
  drop(x%*%inv_Sigma[,d])/drop(sqrt(crossprod(x, inv_Sigma%*%x)))
}

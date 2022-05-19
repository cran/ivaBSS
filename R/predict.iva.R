predict.iva <-
function(object, newdata, which.dataset = NA, ...) {
  if (!is.na(which.dataset)) {
    newdata <- newdata - object$X_means[, which.dataset]
    return(object$W_whitened[, , which.dataset] %*% object$V[, , d] %*% newdata)
  }
  P <- dim(newdata)[1]
  N <- dim(newdata)[2]
  D <- dim(newdata)[3]
  S_hat <- array(NA, c(P, N, D))
  for (d in 1:D) {
    newdata[, , d] <- newdata[, , d] - object$X_means[, d]
    S_hat[, , d] <- object$W_whitened[, , d] %*% object$V[, , d] %*% newdata[, , d]
  }
  return(S_hat)
}

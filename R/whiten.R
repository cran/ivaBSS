whiten <-
function(X) {
  P <- dim(X)[1]
  N <- dim(X)[2]
  D <- dim(X)[3]
  X_centered <- array(0, c(P, N, D))
  Z <- array(0, c(P, N, D))
  V <- array(0, c(P, P, D))
  X_means <- array(0, c(P, D))
  for(d in 1:D) {
    # Whiten the data with BSSprep package
    whitened <- BSSprep(t(X[,,d]))
    # Set centered X
    X_centered[,,d] <- whitened$X.C
    # Set means of X
    X_means[,d] <- whitened$MEAN
    # Set the whitening matrix
    V[,,d] <- whitened$COV.sqrt.i
    # Set whitened data
    Z[,,d] <- t(whitened$Y)
  }
  return(list(Z = Z, V = V, X_centered = X_centered, X_means = X_means))
}

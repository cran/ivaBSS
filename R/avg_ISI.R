avg_ISI <-
function(W, A) {
  D <- length(A[1,1,])
  P <- length(A[,1,1])
  G <- array(NA, dim(A))
  isi <- 0
  for (d in 1:D) {
    Gd <- W[,,d]%*%A[,,d]
    Gabs <- abs(Gd)
    for(i in 1:P) {
      isi <- isi + sum(Gabs[i,])/max(Gabs[i,]) - 1
      isi <- isi + sum(Gabs[,i])/max(Gabs[,i]) - 1
    }
  }
  isi <- isi/(2*P*(P-1)*D)
  return(isi)
}

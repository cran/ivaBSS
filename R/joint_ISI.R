joint_ISI <-
function(W, A) {
  D <- length(A[1,1,])
  P <- length(A[,1,1])
  isi <- 0
  G_abs_sum <- array(0, c(P,P))
  for (d in 1:D) {
    Gd <- W[,,d]%*%A[,,d]
    G_abs_sum <- G_abs_sum + abs(Gd)
  }
  for(i in 1:P) {
    isi <- isi + sum(G_abs_sum[i,])/max(G_abs_sum[i,]) - 1
    isi <- isi + sum(G_abs_sum[,i])/max(G_abs_sum[,i]) - 1
  }
  isi <- isi/(2*P*(P-1))
  return(isi)
}

jbss_achieved <-
function(W, A) {
  successful <- TRUE
  indexes_set <- FALSE
  indexes <- NA
  P <- dim(A)[1]
  D <- dim(A)[3]
  for (d in 1:D) {
    G_k <- W[,,d]%*%A[,,d]
    max_indexes <- apply(abs(G_k), 1, which.max)
    if (length(unique(max_indexes)) < P) {
      successful <- FALSE
    } else {
      if (!indexes_set) {
        indexes <- max_indexes
        indexes_set <- TRUE
      } else if (!all(indexes == max_indexes)) {
        successful <- FALSE
      }
    }
  }
  return(successful)
}

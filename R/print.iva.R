print.iva <-
function(x, ...) {
  cat("Call:", x$call)
  cat("\nData name:", x$DNAME)
  cat("\nThe number of sources P:", x$P)
  cat("\nThe sample size T:", x$N)
  cat("\nThe number of datasets D:", x$D)
  cat("\nSource density model: ", x$source_density)
  if (x$source_density == "student") {
    cat("\nThe degree of freedom:", x$student_df)
  }
  cat("\nThe number of iterations: ", x$niter)
  cat("\nThe algorithm did converge: ", x$converged)
  cat("\nThe means of X (datasets at columns):\n")
  print(x$X_means)
  cat("\nCoefficients:\n")
  print(x$W)
}

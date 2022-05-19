summary.iva <-
function(object, ...) {
  cat("Call:", object$call)
  cat("\nData name:", object$DNAME)
  cat("\nThe number of sources P:", object$P)
  cat("\nThe sample size T:", object$N)
  cat("\nThe number of datasets D:", object$D)
  cat("\nSource density model: ", object$source_density)
  if (object$source_density == "student") {
    cat("\nThe degree of freedom:", object$student_df)
  }
  cat("\nThe number of iterations: ", object$niter)
  cat("\nThe algorithm did converge: ", object$converged)
}

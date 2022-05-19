coef.iva <-
function(object, which.dataset = NA, ...) {
  if (!is.na(which.dataset)) return(object$W[,,which.dataset])
  return(object$W)
}

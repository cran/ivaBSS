components.iva <-
function(object, which.dataset = NA, ...) {
  if (!is.na(which.dataset)) {
    return(object$S[,,which.dataset])
  }
  return(object$S)
}


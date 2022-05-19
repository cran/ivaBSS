\name{predict.iva}
\alias{predict.iva}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict Method for Object of Class iva
}
\description{
Predict the new source estimates best on fitted object of \code{"iva"} class.
}
\usage{
\method{predict}{iva}(object, newdata, which.dataset = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An object of class \code{"iva"}, usually the result of a call to \code{\link{NewtonIVA}} or \code{\link{fastIVA}}.
}
  \item{newdata}{
A numeric data array containing new observed mixtures. Either with dimension \code{[P, N, D]} (if \code{which.dataset = NA}) or \code{[P, N]}, where \code{P} is the number of sources, \code{N} is the number of observations and \code{D} is the number of datasets.
}
  \item{which.dataset}{
Positive integer to determine which dataset is returned. If not set, returns all datasets.
}
  \item{\dots}{
further arguments are not used.
}
}
\details{
The function calculates the source estimates for new observed mixtures based on the model fitted originally. The estimates are zero mean and scaled to unit variance.
}
\value{
Numeric array containing the estimated sources with dimension \code{[P, N]} if \code{which.dataset} is provided and with dimension \code{[P, N, D]} if \code{which.dataset} is not provided.
}


\author{
Mika Sipilä
}
\seealso{
  \code{\link{NewtonIVA}}, \code{\link{fastIVA}}
}
\examples{
if (require("LaplacesDemon")) {
  # Generate sources from multivariate Laplace distribution
  P <- 4; N <- 1000; D <- 4;
  S <- array(NA, c(P, N, D))
  sigmas <- list()

  for (i in 1:P) {
    U <- array(rnorm(D * D), c(D, D))
    sigmas[[i]] <- crossprod(U)
    S[i, , ] <- rmvl(N, rep(0, D), sigmas[[i]])
  }

  # Generate mixing matrices from standard normal distribution
  A <- array(rnorm(P * P * D), c(P, P, D))

  # Generate mixtures
  X <- array(NaN, c(P, N, D))
  for (d in 1:D) {
    X[, , d] <- A[, , d] \%*\% S[, , d]
  }

  # Estimate sources and unmixing matrices
  res_G <- NewtonIVA(X, source_density = "gaussian")

  # Generate new observarions
  N_new <- 10
  S_new <- array(NA, c(P, N_new, D))
  for (i in 1:P) {
    S_new[i, , ] <- rmvl(N_new, rep(0, D), sigmas[[i]])
  }
  X_new <- array(NaN, c(P, N_new, D))
  for (d in 1:D) {
    X_new[, , d] <- A[, , d] \%*\% S_new[, , d]
  }

  # Get source estimates for the new observations
  pred <- predict(res_G, X_new)

  # Get source estimates for only the second dataset
  pred2 <- predict(res_G, X_new[, , 2], which.dataset = 2)
}
}
\keyword{ methods }

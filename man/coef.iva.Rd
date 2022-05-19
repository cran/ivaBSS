\name{coef.iva}
\alias{coef.iva}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Coefficient of the Object of Class iva
}
\description{
\code{coef} method for class \code{"iva"}.
}
\usage{
\method{coef}{iva}(object, which.dataset = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
an object of class \code{"iva"}, usually the result of a call to \code{\link{NewtonIVA}} or \code{\link{fastIVA}}.
}
  \item{which.dataset}{
positive integer. Provides the index in case the unmixing matrix only for a specific data set is desired. Default is to return all unmixing matrices.
}
  \item{\dots}{
further arguments are not used.
}
}
\details{
Returns the unmixing matrices for all datasets or only for the requested dataset.
}
\value{
Unmixing matrix or all unmixing matrices of the object of class \code{"iva"}. If a single unmixing matrix is requested, it is an array with dimension \code{[P, P]} and if all unmixing matrices are requested, it is an array with dimension \code{[P, P, D]}.
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

  for (i in 1:P) {
    U <- array(rnorm(D * D), c(D, D))
    Sigma <- crossprod(U)
    S[i, , ] <- rmvl(N, rep(0, D), Sigma)
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


  # All D unmixing matrices
  coef(res_G)

  # The unmixing matrix for the second dataset
  coef(res_G, 2)
}
}

\keyword{ methods }

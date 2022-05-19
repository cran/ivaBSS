\name{print.iva}
\alias{print.iva}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print an Object of Class iva
}
\description{
\code{print} method for the class \code{"iva"}.
}
\usage{
\method{print}{iva}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{"iva"}, usually the result of a call to \code{\link{NewtonIVA}} or \code{\link{fastIVA}}.
}
  \item{\dots}{
Further arguments are not used.}
}
\details{
The function prints all information of \code{"iva"} object, except the estimated source signals.
}

\value{
No return value, called for printing information of the object of class \code{"iva"}.
}

\author{
Mika Sipilä}
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
  print(res_G)
}
}
\keyword{ print }
\keyword{ methods }

\name{summary.iva}
\alias{summary.iva}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarize an Object of Class iva
}

\description{
\code{summary} method for the class \code{"iva"}.
}

\usage{
\method{summary}{iva}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An object of class \code{"iva"}, usually the result of a call to \code{\link{NewtonIVA}} or \code{\link{fastIVA}}.
}
  \item{\dots}{
Further arguments are not used.}
}

\details{
The function print all the information of the \code{"iva"} object except the estimated sources and the estimated unmixing matrices.
}

\value{
No return value, called for summarizing the object of class \code{"iva"}.
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
  summary(res_G)
}
}

\keyword{ methods }

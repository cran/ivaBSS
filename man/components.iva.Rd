\name{components.iva}
\alias{components.iva}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Components of the Object of Class iva
}
\description{
  Returns the estimated source components of object of class \code{"iva"}.
}
\usage{
  components.iva(object, which.dataset = NA, ...)
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
  Returns the estimated source components for all datasets or only for the requested dataset.
}
\value{
  Estimated source components for requested dataset or for all datasets of the object of class \code{"iva"}. If a single dataset is requested, it is an array with dimension \code{[P, N]} and if all datasets are requested, it is an array with dimension \code{[P, N, D]}.
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


  # Source estimates for all D datasets
  components.iva(res_G)

  # Source estimates for the second dataset
  components.iva(res_G, 2)
}
}

\keyword{ methods }

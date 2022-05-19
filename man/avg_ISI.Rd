\name{avg_ISI}
\alias{avg_ISI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Average Intersymbol Inference
}
\description{
Calculates the average intersymbol inference for two sets of matrices.
}
\usage{
avg_ISI(W, A)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{W}{
Array of unmixing matrices with dimension \code{[P, P, D]}.
}
  \item{A}{
Array of true mixing matrices with dimension \code{[P, P, D]}.
}
}
\details{
The function returns the average intersymbol inference for the set of estimated unmixing matrices and the set of true mixing matrices. The average ISI gets the value between 0 and 1, where 0 is the optimal result. The average ISI is calculated as the mean ISI over each dataset separately. The average ISI does not take the permutation of the estimated sources into account.
}
\value{
Numeric value between 0 and 1, where 0 is the optimal result indicating that the sources are separated perfectly in each dataset.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
\cite{Anderson, M. (2013). Independent vector analysis: Theory, algorithms, and applications.
PhD dissertation, University of Maryland, Baltimore County.}
}
\author{
Mika Sipilä
}

\seealso{
\code{\link{joint_ISI}}, \code{\link{jbss_achieved}}
}
\examples{
# Mixing matrices and unmixing matrices generated
# from standard normal distribution
P <- 4; D <- 4;
W <- array(rnorm(P * P * D), c(P, P, D))
A <- array(rnorm(P * P * D), c(P, P, D))

avg_ISI(W, A)

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
  avg_ISI(coef(res_G), A)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{multivariate}

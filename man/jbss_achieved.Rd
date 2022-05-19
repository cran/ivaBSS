\name{jbss_achieved}
\alias{jbss_achieved}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
JBSS Achieved
}
\description{
The function calculates if the joint blind source separation (JBSS) is achieved.
}
\usage{
jbss_achieved(W, A)
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
The function calculates if the joint blind source separation is achieved. JBSS is considered achieved when the the location of maximum absolute values of each row of gain matrix
\code{G[,,d] = W[,,d] \%*\% A[,,d]} is unique within the dataset, but shared between the datasets \code{1, ...,D}. The first indicates that the sources are separated within dataset and the second indicates that the estimated sources are aligned in same order for each dataset.
}
\value{
Logical. If \code{TRUE} the JBSS is considered achieved.
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
\code{\link{joint_ISI}}, \code{\link{avg_ISI}}
}
\examples{
# Mixing matrices and unmixing matrices generated
# from standard normal distribution
P <- 4; D <- 4;
W <- array(rnorm(P * P * D), c(P, P, D))
A <- array(rnorm(P * P * D), c(P, P, D))

jbss_achieved(W, A)

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
  jbss_achieved(coef(res_G), A)
}
}

\keyword{multivariate}

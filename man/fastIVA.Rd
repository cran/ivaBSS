\name{fastIVA}
\alias{fastIVA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fast Fixed-point IVA Algorithm
}
\description{
The algorithm estimates the sources from multiple dependent datasets
jointly using their observed mixtures. The estimation is done by
maximizing the independence between the sources, when the estimated unmixing matrices are restricted to be orthogonal. The options for different source densities are provided.
}
\usage{
fastIVA(X, source_density="laplace_diag", student_df=1,
max_iter = 1024, eps = 1e-6, W_init = NA, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{numeric data array containing the observed mixtures with dimension \code{[P, N, D]},
  where \code{P} is the dimension of the observed dataset, \code{N} is the number of the observations
  and \code{D} is the number of the datasets. The number of datasets \code{D} should be at least 2. Missing values are not allowed.}
  \item{source_density}{
string to determine which source density model should be used. The options are \code{"laplace_diag"}, \code{"student"} or \code{"entropic"}. For more information see the details section.
}
  \item{student_df}{integer.
The degree of freedom for multivariate Student's distribution. Used only if \code{source_denisty = "student"}.
}
\item{max_iter}{positive integer, used to define the maximum number of iterations for algorithm to run. If \code{max_iter} is reached, the unmixing matrices of the last iteration are used.
}
  \item{eps}{
convergence tolerance, when the convergence measure is smaller than \code{eps}, the algorithm stops.
}
  \item{W_init}{
numeric array of dimension \code{[P, P, D]} containing initial unmixing matrices. If not set, initialized with identity matrices.
}
  \item{verbose}{logical. If \code{TRUE} the convergence measure is printed during the learning process.}
}
\details{
The algorithm uses fixed-point iteration to estimate to estimate the multivariate source signals from their observed mixtures. The elements of the source signals, or the datasets, should be dependent of each other to achieve the estimates where the sources are aligned in same order for each dataset. If the datasets are not dependent, the sources can still be separated but not necessarily aligned. This algorithm restricts the estimates unmixing matrices to be orthogonal. For more of the fast fixed-point IVA algorithm, see Lee, I. et al (2007).

The source density model should be selected to match the density of the true source signals. When \code{source_density = "laplace_diag"}, the multivariate Laplace source density model with diagonal covariance structure is used. When \code{source_density = "entropic"}, the approximated entropy based source density model is used. For more about multivariate Laplace and entropic source density models, see Lee, I. et al (2007).
When \code{source_density = "student"} the multivariate Student's source density model is used, for more see Liang, Y. et al (2013).

The algorithm assumes that observed signals are multivariate, i.e. the number of datasets \code{D >= 2}. The estimated signals are zero mean and scaled to unit variance.
}
\value{
An object of class \code{"iva"}.
%%  If it is a LIST, use
\item{S}{The estimated source signals with dimension \code{[P, N, D]}. The estimated source signals are zero mean with unit variance.}
\item{W}{The estimated unmixing matrices with dimension \code{[P, P, D]}.}
\item{W_whitened}{The estimated unmixing matrices with dimension \code{[P, P, D]} for whitened data.}
\item{V}{The whitening matrices with dimension \code{[P, P, D]}.}
\item{X_means}{The means for each observed mixture with dimension \code{[P, D]}.}
\item{niter}{The number of iterations that the algorithm did run.}
\item{converged}{Logical value which tells if the algorithm converged.}
\item{source_density}{The source density model used.}
\item{N}{The number of observations.}
\item{D}{The number of datasets.}
\item{P}{The number of sources.}
\item{student_df}{The degree of freedom for Student's source density model.}
\item{call}{The function call.}
\item{DNAME}{The name of the variable containing the observed mixtures.}
%% ...
}
\references{
\cite{Lee, I., Kim, T., & Lee, T.-W. (2007). Fast fixed-point independent vector analysis algorithms
for convolutive blind source separation. Signal Processing, 87, 1859–1871.
<doi:10.1016/j.sigpro.2007.01.010>}

\cite{Liang, Y., Chen, G., Naqvi, S., & Chambers, J. A. (2013). Independent vector analysis
with multivariate Student’s t-distribution source prior for speech separation. Electronics
Letters, 49, 1035–1036. <doi:10.1049/el.2013.1999>}
}
\author{
Mika Sipilä
}

\seealso{
\code{\link{NewtonIVA}}
}
\examples{
if (require("LaplacesDemon")) {
  # Generate sources from multivariate Laplace distribution
  P <- 2; N <- 1000; D <- 5;
  S <- array(NA, c(P, N, D))

  for (i in 1:P) {
    S[i, , ] <- rmvl(N, rep(0, D), diag(D))
  }

  # Generate mixing matrices from standard normal distribution
  A <- array(rnorm(P * P * D), c(P, P, D))

  # Generate mixtures
  X <- array(NaN, c(P, N, D))
  for (d in 1:D) {
    X[, , d] <- A[, , d] \%*\% S[, , d]
  }

  # Estimate sources and unmixing matrices
  res <- fastIVA(X)
}
}

\keyword{ multivariate }

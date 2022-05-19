\name{NewtonIVA}
\alias{NewtonIVA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Newton Update Based IVA Algorithm
}
\description{
The algorithm estimates the sources from multiple dependent datasets
jointly using their observed mixtures. The estimation is done by
maximizing the independence between the sources. The options for different source densities are provided.
}
\usage{
NewtonIVA(X, source_density="laplace", student_df=1,
init = "default", max_iter = 1024, eps = 1e-6, W_init = NA,
step_size=1, step_size_min = 0.1, alpha = 0.9, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{numeric data array containing the observed mixtures with dimension \code{[P, N, D]},
  where \code{P} is the dimension of the observed dataset, \code{N} is the number of the observations
  and \code{D} is the number of the datasets. The number of datasets \code{D} should be at least 2. Missing values are not allowed.}
  \item{source_density}{
string to determine which source density model should be used. The options are \code{"laplace"}, \code{"laplace_diag"}, \code{"gaussian"} or \code{"student"}. For more information see the details section.
}
  \item{student_df}{integer.
The degree of freedom for multivariate Student's distribution. Used only if \code{source_denisty = "student"}.
}
\item{init}{string, to determine how to initialize the algorithm. The options are
\code{"default"}, \code{"IVA-G+fastIVA"}, \code{"IVA-G"}, \code{"fastIVA"} or \code{"none"}. For more information see the details section.}
  \item{max_iter}{positive integer, used to define the maximum number of iterations for algorithm to run. If \code{max_iter} is reached, the unmixing matrices of the last iteration are used.
}
  \item{eps}{
convergence tolerance, when the convergence measure is smaller than \code{eps}, the algorithm stops.
}
  \item{W_init}{
numeric array of dimension \code{[P, P, D]} containing initial unmixing matrices. If not set, initialized with identity matrices.
}
  \item{step_size}{
initial step size for Newton step, should be between 0 and 1, default is 1.
}
  \item{step_size_min}{
the minimum step size.
}
  \item{alpha}{
multiplier for how much to decrease step size when convergence is not getting smaller.
}
  \item{verbose}{logical. If \code{TRUE} the convergence measure is printed during the learning process.}
}
\details{
The algorithm uses Newton update together with decoupling trick to estimate the multivariate source signals from their observed mixtures. The elements of the source signals, or the datasets, should be dependent of each other to achieve the estimates where the sources are aligned in same order for each dataset. If the datasets are not dependent, the sources can still be separated but not necessarily aligned. The algorithm does not assume the unmixing matrices to be orthogonal. For more of the nonorthogonal Newton update based IVA algorithm, see Anderson, M. et al (2011) and Anderson, M. (2013).

The source density model should be selected to match the density of the true source signals. When \code{source_density = "laplace"}, the multivariate Laplace source density model is used. This is the most flexible choice as it takes both second-order and higher-order dependence into account.

When \code{source_density = "laplace_diag"}, the multivariate Laplace source density model with diagonal covariance structure is used. Multivariate diagonal Laplace source density model should be considered only when the sources are mainly higher-order dependent. It works best when the number of sources is significantly less than the number of datasets.

When \code{source_density = "gaussian"} the multivariate Gaussian source density model is used. This is the superior choice in terms of computation power and should be used when the sources are mostly second-order dependent.

When \code{source_density = "student"} the multivariate Student's source density model is used. Multivariate Student's source density model should be considered only when the sources are mainly higher-order dependent. It works best when the number of sources is significantly less than the number of datasets.

The \code{init} parameter defines how the algorithm is initialized. When \code{init = "default"}, the default initialization is used. As default the algorithm is initialized using \code{init = "IVA-G+fastIVA"} when \code{source_density} is \code{"laplace"}, \code{"laplace_diag"} or \code{"student"}, and using \code{init = "none"} when \code{source_density = "gaussian"}.

When \code{init = "IVA-G+fastIVA"}, the algorithm is initialized using first the estimated unmixing matrices of IVA-G, which is \code{NewtonIVA} with \code{source_density = "gaussian"}, to initialize \code{fastIVA} algorithm. Then the estimated unmixing matrices \code{W} of \code{fastIVA} are used as initial unmixing matrices for \code{NewtonIVA}. IVA-G is used to solve the permutation problem of aligning the source estimates when ever the true sources are second-order dependent. If the true sources are not second-order dependent, \code{fastIVA} is used as backup as it solves the permutation problem more regularly than \code{NewtonIVA} when the sources are purely higher-order dependent. When the sources possess any second-order dependence, IVA-G also speeds the computation time up a lot. This option should be used whenever there is no prior information about the sources and \code{source_density} is either \code{"laplace"}, \code{"laplace_diag"} or \code{"student"}.

When \code{init = "IVA-G"}, the estimated unmixing matrices of IVA-G are used to initialize this algorithm. This option should be used if the true sources are expected to possess any second-order dependence and \code{source_density} is not \code{"gaussian"}.

When \code{init = "fastIVA"}, the estimated unmixing matrices of \code{fastIVA} algorithm is used to initialize this algorithm. This option should be used if the true sources are expected to possess only higher-order dependence. For more details, see \code{\link{fastIVA}}.

When \code{init = "none"}, the unmixing matrices are initialized randomly from standard normal distribution.

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
\cite{Anderson, M., Adalı, T., & Li, X.-L. (2011). Joint blind source separation with multivariate
Gaussian model: Algorithms and performance analysis. IEEE Transactions on Signal
Processing, 60, 1672–1683. <doi:10.1109/TSP.2011.2181836>}

\cite{Anderson, M. (2013). Independent vector analysis: Theory, algorithms, and applications.
PhD dissertation, University of Maryland, Baltimore County. }

\cite{Liang, Y., Chen, G., Naqvi, S., & Chambers, J. A. (2013). Independent vector analysis
with multivariate Student’s t-distribution source prior for speech separation. Electronics
Letters, 49, 1035–1036. <doi:10.1049/el.2013.1999>}
}
\author{
Mika Sipilä
}

\seealso{
\code{\link{fastIVA}}
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
}
}

\keyword{ multivariate }

\name{plot.iva}
\alias{plot.iva}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting an Object of Class iva
}
\description{
\code{plot} method for the class \code{"iva"}.
}
\usage{
\method{plot}{iva}(x, which.dataset = NA, which.source = NA,
type = "l", xlabs = c(), ylabs = c(), colors = c(),
oma = c(1, 1, 0, 0), mar = c(2, 2, 1, 1), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{"iva"}, usually the result of a call to \code{\link{NewtonIVA}} or \code{\link{fastIVA}}.
}
  \item{which.dataset}{
Positive integer to determine which dataset is returned. If not set, returns all datasets.
}
  \item{which.source}{
Positive integer to determine which dataset is returned. If not set, returns all datasets.
}
  \item{type}{
1-character string giving the type of plot desired. For details, see \code{\link[base]{plot}}.
}
  \item{xlabs}{
Vector containing the labels for x-axis.
}
  \item{ylabs}{
Vector containing the labels for y-axis.
}
  \item{colors}{
Vector containing the colors for each plot.
}
  \item{oma}{
A vector of the form \code{c(bottom, left, top, right)} giving the size of the outer margins in lines of text. For more details, see \code{\link[=stats]{par}}.
}
  \item{mar}{
A numerical vector of the form \cite{c(bottom, left, top, right)} which gives the number of lines of margin to be specified on the four sides of the plot. For more details, see \code{\link[=stats]{par}}.
}
  \item{\dots}{
Further arguments passed to \code{\link[=stats]{plot}} function.
}
}
\details{
Plots either all estimated sources of the object of class \code{"iva"} or the estimates for specific dataset and/or source.
}

\value{
No return value, called for plotting the estimated sources of the object of class \code{"iva"}.
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

  # Plot all estimated sources
  plot(res_G)

  # Plot the source estimates for the first dataset only
  plot(res_G, which.dataset = 1)

  # Plot the source estimates for the second source only
  plot(res_G, which.source = 2)

  # Plot the source estimate of the second dataset and third source
  plot(res_G, which.dataset = 2, which.source = 3, type = "p")

  # Plot all source estimates with custom colors and labels
  plot(res_G, col=c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4)),
      xlabs = c("Subject 1", "Subject 2", "Subject 3", "Subject 4"),
      ylabs = c("Channel 1", "Channel 2", "Channel 3", "Channel 4"))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ methods }

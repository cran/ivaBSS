## R Package for Independent Vector Analysis

Independent vector analysis (IVA) is a blind source separation (BSS) model where several datasets are jointly unmixed. This package provides several methods for the unmixing together with some performance measures.

## How to install the package?

Make sure you have git installed and clone the package using:

`git clone https://github.com/mikasip/IVA.git`

or just download `ivaBSS_1.0.0.tar.gz` file from this repository.

Make sure you have R in your environment variables, open command prompt and run:

`R CMD INSTALL path_to_file/ivaBSS_1.0.0.tar.gz`

## How to use?

The package is used to estimate source vectors by unmixing the observed mixtures. The next example generates mixtures from sources following multivariate Laplace distribution and unmixes them using Newton update based IVA with multivariate Gaussian source density model.

```
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

```

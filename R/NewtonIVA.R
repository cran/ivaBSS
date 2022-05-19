NewtonIVA <-
function(X, source_density="laplace", student_df=1, init = "default", max_iter = 1024, eps = 1e-6, W_init = NA, step_size=1, step_size_min = 0.1, alpha = 0.9, verbose = FALSE) {

  source_density <- match.arg(source_density, c("laplace", "laplace_diag", "gaussian", "student"))
  init <- match.arg(init, c("default", "IVA-G+fastIVA", "IVA-G", "fastIVA", "none"))
  if (init == "default") init <- switch(source_density, "gaussian" = "none", "IVA-G+fastIVA")
  D <- dim(X)[3]
  P <- dim(X)[1]
  N <- dim(X)[2]

  whitened <- whiten(X)
  Z <- whitened$Z
  V <- whitened$V

  # Initialize W
  if (!is.array(W_init)) {
    W <- switch(init,
           "IVA-G+fastIVA" = {
             res_G <- NewtonIVA(X, "gaussian", verbose = verbose)
             res_fast <- fastIVA(X, source_density, student_df, max_iter, W_init = res_G$W_whitened, verbose = verbose)
             res_fast$W_whitened
           },
           "IVA-G" = {
             res_G <- NewtonIVA(X, "gaussian", verbose = verbose)
             res_G$W_whitened
           },
           "fastIVA" = {
             res_fast <- fastIVA(X, source_density, student_df, max_iter, verbose = verbose)
             res_fast$W_whitened
           },
           "none" = {
             array(rnorm(P*P*D), c(P,P,D))
           })
  } else if(all(dim(W_init) == c(P,P,D))) {
    W <- W_init
  } else {
    warning("W_init is not in correct shape, identity matrices usedas initial W instead.")
    W <- array(rep(diag(nrow=P, ncol=P),D), c(P,P,D))
  }

  # Initialize the source density model
  if (source_density == "student") {
    dG <- dG_student
    d2G <- d2G_student
    theta <- student_df
  }
  else if (source_density == "laplace") {
    dG <- dG_laplace
    d2G <- d2G_laplace
  } else if (source_density == "laplace_diag") {
    dG <- dG_laplace_diag
    d2G <- d2G_laplace_diag
  }

  # Initialize the source signal estimates
  S <- array(0, c(P,N,D))
  for(d in 1:D) {
    S[,,d] <- W[,,d]%*%Z[,,d]
  }

  # Calculate the covariance matrices of Z[,,d1] and Z[,,d2], d1,d2=1,...D, for faster Sigma estimation:
  Rx <- matrix(list(), D, D)
  for (d1 in 1:D) {
    for (d2 in d1:D) {
      Rx_d1d2 <- tcrossprod(Z[,,d1],Z[,,d2])/N
      Rx[[d1,d2]] <- Rx_d1d2
      if (d2 != d1) {
        Rx[[d2,d1]] <- t(Rx_d1d2)
      }
    }
  }

  tol <- 0
  tol_down_counter <- 0
  converged <- FALSE
  # The main iteration loop:
  for(iter in 1:max_iter) {

    prev_tol <- tol
    W_old <- W

    # Update W
    for(j in 1:P) {

      # Update the source density distribution parameters
      if(source_density %in% c("gaussian", "laplace")) {
        Sigma_n <- diag(1, D)
        for (d1 in 1:D) {
          for (d2 in d1:D) {
            Sigma_n[d1,d2] <- crossprod(W[j,,d1], Rx[[d1,d2]]%*%W[j,,d2])
            if (d1 != d2) {
              Sigma_n[d2,d1] <- t(Sigma_n[d1,d2])
            }
          }
        }
        theta <- solve(Sigma_n)
      }

      for(d in 1:D) {
        # The decoupling trick
        if (P == 2) {
          h <- array(c(-1/W[-j,1,d], 1/W[-j,2,d]), c(2,1))
        }
        else {
          temp1 <- rnorm(P)
          h <- temp1 - crossprod(W[-j,,d], solve(tcrossprod(W[-j,,d]),W[-j,,d]))%*%temp1
        }
        h <- h/norm(h)

        # Calculate the gradient and the inverse of the Hessian matrix
        if (source_density == "gaussian") {
          E_xs <- array(0, c(P,D))
          for (dd in 1:D) {
            E_xs[,dd] <- Rx[[d,dd]]%*%W[j,,dd]
          }
          dG_vals <- E_xs%*%theta[,d]
          d2G_vals <- Rx[[d,d]]*theta[d,d]
        } else {
          dG_vals <- rowMeans(sapply(1:N, FUN = function(i) { dG(S[j,i,d], S[j,i,], d, theta)*Z[,i,d]}))
          d2G_vals <- rowMeans(sapply(1:N, FUN = function(i) {
            d2G(S[j,i,d], S[j,i,], d, theta)*tcrossprod(Z[,i,d])
          }))
        }
        grad <- -h/drop(W[j,,d]%*%h) + dG_vals
        inv_hess <- matrix(d2G_vals, nrow=P, byrow=TRUE) + tcrossprod(h)/drop(crossprod(h,W[j,,d]))^2
        # The newton step
        new_val <- W[j,,d] - step_size*solve(inv_hess,grad)
        # The normalization of new W[j,,d]
        W[j,,d] <- new_val/sqrt(sum(new_val^2))
      }
    }

    # Calculate new source estimates
    for (d in 1:D) {
      S[,,d] <- W[,,d]%*%Z[,,d]
    }
    # Calculate the value the convergence measure
    tol <- 0
    for (d in 1:D) {
      tol <- max(tol, 1 - min(diag(tcrossprod(W_old[,,d], W[,,d]))))
    }
    if (verbose) {
      print(paste0("Convergence measure: ", tol))
    }
    # Modify step_size if needed
    if (iter > 1) {
      if (tol > prev_tol) {
        step_size <- max(step_size*alpha, step_size_min)
        tol_down_counter <- 0
      } else {
        tol_down_counter <- tol_down_counter + 1
      }
      if (tol_down_counter >= 5) {
        step_size <- min(step_size/alpha, 1)
        tol_down_counter <- 0
      }
    }

    # Break the iteration loop if converged
    if(eps > tol) {
      converged <- TRUE
      break
    }
  }

  W_nonwhitened <- array(0, dim(W))
  #Dewhiten the unmixing matrices and calculate the final source estimates
  for (d in 1:D) {
    W_nonwhitened[,,d] <- W[,,d]%*%V[,,d]
  }
  dimnames(S)[[1]] <- sapply(1:P, FUN = function(j) paste0("IC.", j))
  RES <- list(S = S, W = W_nonwhitened, W_whitened = W, V = V, X_means = whitened$X_means, niter = iter, converged = converged, source_density = source_density, N = N, D = D, P = P, student_df = student_df, call = deparse(sys.call()), DNAME = paste(deparse(substitute(X))))
  class(RES) <- "iva"
  return(RES)
}

plot.iva <-
function(x, which.dataset = NA, which.source = NA, type="l", xlabs = c(), ylabs = c(), colors = c(), oma = c(1, 1, 0, 0), mar = c(2, 2, 1, 1), ...) {

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  S <- x$S
  if (!is.na(which.dataset)) {
    S <- array(S[,,which.dataset], c(dim(S)[1], dim(S)[2],1))
  }
  if (!is.na(which.source)) {
    S <- array(S[which.source,,], c(1, dim(S)[2], dim(S)[3]))
  }
  P <- dim(S)[1]
  D <- dim(S)[3]
  par(mfrow = c(P, D),
    oma = oma,
    mar = mar,
    mgp = c(2, 0.5, 0),
    xpd = NA)
  for (d in 1:D) {
    if (length(xlabs) < d) xlabs[d] <- paste0("Dataset ", ifelse(is.na(which.dataset), d, which.dataset))
  }
  for (i in 1:P) {
    if (length(ylabs) < i) ylabs[i] <- paste0("Estimate ", ifelse(is.na(which.source), i, which.source))
  }
  for(i in 1:P) {
    for (d in 1:D) {
      if (is.null(colors)) {
        col <- 1
      } else {
        col <- ifelse(is.na(colors[(i - 1)*D + d]), 1,  colors[(i - 1)*D + d])
      }
      plot(S[i,,d], type=type, xlab = ifelse(i == P, xlabs[d], ""), ylab = ifelse(d == 1, ylabs[i], ""), col = col,...)
    }
  }
}

Qfun <- function(M. , marg){

  coefs. <- M.cal.matrix(marg)
  coefs. <- coefs. + diag(1, nrow(coefs.))

  Q.vector <- list()

  for (i in seq_len(nrow(coefs.))) {
    Q. <- matrix(0, nrow = nrow(M.[[1]]), ncol = ncol(M.[[1]]))
    coefs.vec <- as.vector(coefs.[i,])
    for (j in seq_along(coefs.vec)) {
      if (coefs.vec[j] != 0){
        Q. <- Q. + coefs.vec[j] * M.[[j]]
      }
    }
    Q.vector[[i]] <- Q.
  }
  return(Q.vector)
}

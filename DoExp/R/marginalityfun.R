marginalityfun <- function(List) {
  names <- names(List)
  n <- length(names)
  marg <- matrix(0, nrow = n, ncol = n, dimnames = list(names, names))

  for (i in names) {
    for (j in names) {
      A <- List[[i]]
      B <- List[[j]]
      result <- A %*% B %*% A - B
      marg[i, j] <- ifelse(all(round(result, 10) == 0), 1, 0)
    }
  }

  return(marg)
}

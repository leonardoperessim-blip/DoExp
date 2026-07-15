Xfun <- function(nn., data) {
  X. <- list()
  X.[["Universe"]] <- matrix(1, nrow = nrow(data), ncol = 1)

  for (name in nn.$names[-1]) {
    form <- stats::reformulate(name, intercept = FALSE)
    matrix. <- stats::model.matrix(form, data = data)
    matrix. <- matrix.[, colSums(matrix.) != 0, drop = FALSE]
    X.[[name]] <- matrix.
  }

  return(X.)
}

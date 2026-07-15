M.cal.matrix <- function(marginality.matrix) {
  n <- nrow(marginality.matrix)
  I <- diag(n)
  rownames(I) <- colnames(I) <- rownames(marginality.matrix)

  M <- matrix(0, nrow = n, ncol = n)
  rownames(M) <- colnames(M) <- rownames(marginality.matrix)

  M[1,1] <- 1

  for (i in 2:n) {
    v <- I[i, ]
    marginals <- which(marginality.matrix[i, ] == 1 & (1:n) < i)

    for (j in names(marginals)) {
      v <- v - M[j, ]
    }

    M[i, ] <- v

  }

  diag(M) <- 0

  return(M)
}

datafactorfun <- function(df) {
  for (j in seq_along(df)) {
    x <- df[[j]]
    if (!is.factor(x)) df[[j]] <- factor(x)
  }
  df
}

dffun <- function(M.,marg){
  df.vector <- data.frame(levels = rep(NA, length(M.)), df = rep(NA, length(M.)))
  for (i in 1:length(M.)) {
    matriz <- M.[[i]]
    df.vector$levels[[i]] <- sum(diag(matriz))
    df.vector$df[[i]] <- sum(diag(matriz))
  }
  R1 <- coefsfun(marg)
  rownames(R1) <- colnames(R1) <- as.numeric(df.vector$levels)
  for (i in seq_len(nrow(df.vector))) {
    coef_lin <- R1[i, ]
    sum_mult <- sum(as.numeric(colnames(R1)) * as.numeric(coef_lin))
    df.vector$df[i] <- df.vector$df[i] + sum_mult
  }
  return(df.vector)
}

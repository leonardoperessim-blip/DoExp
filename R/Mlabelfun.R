Mlabelfun <- function(M., marg,name.){
  M.vector <- data.frame(M = rep(NA, length(M.)), core = rep(NA, length(M.)))
  M.vector[1 & 2] <- "M[G]"
  for (i in 2:length(M.)) {
    M_sym <- "M"
    sub_raw <- name.$label_left[i - 1]
    sub_parsed <- paste0('"', sub_raw, '"')
    M.vector$M[[i]]    <- paste0(M_sym, "[", sub_parsed, "]")
    M.vector$core[[i]] <- paste0(M_sym, "[", sub_parsed, "]")
  }
  T1 <- coefsfun(marg)
  rownames(T1) <- colnames(T1) <- M.vector$M
  for (i in seq_len(nrow(M.vector))) {
    term <- M.vector$M[i]
    coefs <- T1[term, ]
    extraterm <- character(0)
    for (j in names(coefs)) {
      coef <- coefs[[j]]
      if (coef == 0) next
      signal <- ifelse(coef > 0, "+ ", "- ")
      if (abs(coef) == 1) {
        extraterm <- c(extraterm, paste0(signal, j))
      } else {
        extraterm <- c(extraterm, paste0(signal, abs(coef), j))
      }
    }

    if (length(extraterm) > 0) {
      M.vector$core[i] <- paste(M.vector$core[i], paste(extraterm, collapse = " "))
    }
  }
  return(M.vector)
}

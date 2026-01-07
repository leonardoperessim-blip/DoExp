coefEMS <- function(term_name, M.) {
  M <- M.[[term_name]]
  if (is.null(M)) return(NA_real_)
  den <- sum(diag(M))
  if (isTRUE(den == 0)) return(NA_real_)
  nrow(M) / den
}

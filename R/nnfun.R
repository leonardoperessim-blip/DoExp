nnfun <- function(form) {
  form_  <- stats::as.formula(paste("~", form))
  trm    <- stats::terms(form_)
  labels <- attr(trm, "term.labels")
  k <- length(labels)

  nn. <- list(
    numbers = 0:k,
    names   = c("Universe", labels)
  )

  return(nn.)
}

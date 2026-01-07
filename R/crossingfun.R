crossingfun <- function(L, R) {
  mains <- c(L, R)
  Lstar <- L
  Rstar <- R
  inter <- unlist(lapply(Lstar, function(a)
    lapply(Rstar, function(b) list(
      kind  = "inter",
      fmt   = paste0(a$fmt, "#", b$fmt),
      depth = max(a$depth, b$depth)
    ))
  ), recursive = FALSE)
  c(mains, inter)
}

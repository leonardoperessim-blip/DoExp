Mfun <- function(X.){
  M. <- list()
  for (name in names(X.)) {
    M.[[name]] <- X.[[name]]%*%solve(t(X.[[name]])%*%X.[[name]])%*%t(X.[[name]])
  }
  return(M.)
}

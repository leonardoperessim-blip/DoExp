parsetree2 <- function(formula_str) {
  splited <- splitfun(formula_str)
  parsetree1(splited)
}

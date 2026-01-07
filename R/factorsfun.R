factorsfun <- function(node) {
  if (node$type == "factor") return(node$name)
  if (node$type == "nest")   return(c(factorsfun(node$parent), factorsfun(node$child)))
  if (node$type %in% c("cross","sum")) {
    return(unique(c(factorsfun(node$left), factorsfun(node$right))))
  }
  character(0)
}

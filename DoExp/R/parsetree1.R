parsetree1 <- function(splited, position = 1L) {
  parse_primary <- function() {
    t <- splited[position,]
    if (t$type == "ID") {
      node <- list(type = "factor", name = t$tokens)
      position <<- position + 1L
      return(node)
    }
    if (t$tokens == "(") {
      position <<- position + 1L
      node <- parse_sum()
      if (position > nrow(splited) || splited[position, "tokens"] != ")") stop("Unclosed parenthesis.")
      position <<- position + 1L
      return(node)
    }
    stop(sprintf("Unexpected: '%s'", t$tokens))
  }
  parse_nest <- function() {
    left <- parse_primary()
    while (position <= nrow(splited) && splited[position, "tokens"] == "/") {
      position <<- position + 1L
      right <- parse_primary()
      left <- list(type = "nest", parent = left, child = right)
    }
    left
  }
  parse_cross <- function() {
    left <- parse_nest()
    while (position <= nrow(splited) && splited[position, "tokens"] == "*") {
      position <<- position + 1L
      right <- parse_nest()
      left <- list(type = "cross", left = left, right = right)
    }
    left
  }
  parse_sum <- function() {
    left <- parse_cross()
    while (position <= nrow(splited) && splited[position, "tokens"] == "+") {
      position <<- position + 1L
      right <- parse_cross()
      left <- list(type = "sum", left = left, right = right)
    }
    left
  }
  syntaxtree <- parse_sum()
  if (position <= nrow(splited)) stop(sprintf("leftover: '%s'", splited[position,"tokens"]))
  syntaxtree
}

hassefun <- function(nn., form) {
  formula. <- stats::as.formula(paste("~", form))
  n <- length(nn.$numbers)

  hasse. <- data.frame(
    name    = nn.$names,
    stratum = c(0, attr(stats::terms(formula.), "order")),
    x       = rep(NA, n),
    y       = rep(NA, n)
  )

  hasse.[1, "x"] <- 60
  hasse.[1, "y"] <- 0

  for (i in 2:n) {
    hasse.$y[i] <- 30 * hasse.$stratum[i]
  }

  hasse. <- dplyr::group_by(hasse., .data$stratum)
  hasse. <- dplyr::mutate(
    hasse.,
    n.stratum = dplyr::n(),
    x = if (dplyr::n() > 1) {
      seq(60 - 21 * (dplyr::n() - 1) / 2, 60 + 21 * (dplyr::n() - 1) / 2, length.out = dplyr::n())
    } else {
      60
    }
  )
  hasse. <- dplyr::ungroup(hasse.)

  hasse. <- dplyr::mutate(hasse., y = .data$y * -1)

  return(hasse.)
}

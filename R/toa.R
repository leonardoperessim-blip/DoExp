#' toa
#'
#' Returns the Anatomy Table with degrees of freedom and the a-effiency for each
#' source of variation for the ANOVA model, from the structural formulas of units
#' and tratments and a data.frame.
#'
#' @param units a structural formula of units as a character.
#' @param trt a structural formula of treatments as a character.
#' @param data data.frame containing the variables named in the structural formulas.
#'
#' @examples
#' if (requireNamespace("dae", quietly = TRUE)) {
#'   blocks <- dae::fac.gen(list(
#'     Blocks = 4,
#'     A = 2,
#'     C = 3
#'   ))
#'
#'   blocks$Plots <- rep(1:6, times = 4)
#'
#'   anatomytableCBD <- toa(
#'     units = "Blocks/Plots",
#'     trt = "A*C",
#'     data = blocks
#'   )
#'
#'   BIBD <- data.frame(
#'     Blocks = factor(gl(10, 3)),
#'     Plots = factor(rep(1:3, times = 10)),
#'     Treat = factor(c(
#'       1, 2, 3, 1, 2, 4, 1, 2, 5, 1,
#'       3, 4, 1, 3, 5, 1, 4, 5, 2, 3,
#'       4, 2, 3, 5, 2, 4, 5, 3, 4, 5
#'     ))
#'   )
#'
#'   anatomytableBIBD <- toa(
#'     units = "Blocks/Plots",
#'     trt = "Treat",
#'     data = BIBD
#'   )
#' }

#'@export
toa <- function(units = NULL, trt = NULL, data = NULL) {

  data <- datafactorfun(df = data)

  nn.units <- nnfun(form = units)
  nn.trt <- nnfun(form = trt)

  X.units <- Xfun(nn.=nn.units, data = data)
  X.trt <- Xfun(nn.=nn.trt, data = data)

  M.units <- Mfun(X.=X.units)
  M.trt <- Mfun(X.=X.trt)

  marginality.unit <- marginalityfun(M.units)
  marginality.trt <- marginalityfun(M.trt)

  hasse.unit <- tablefun(nn.=nn.units, form = units)
  hasse.trt  <- tablefun(nn.=nn.trt,  form = trt)

  labels.unit <- parsetree5(units)
  labels.trt  <- parsetree5(trt)

  labels.unit1 <- dplyr::mutate(
    dplyr::filter(hasse.unit, name != "Universe"),
    term = purrr::map_chr(name, ~ matchfun(.x, labels.unit$term))
  )

  labels.trt1 <- dplyr::mutate(
    dplyr::filter(hasse.trt, name != "Universe"),
    term = purrr::map_chr(name, ~ matchfun(.x, labels.trt$term))
  )

  Q.units <- Qfun(M. = M.units, marg = marginality.unit)
  Q.trt   <- Qfun(M. = M.trt,   marg = marginality.trt)

  Q.units <- tail(Q.units, -1)
  Q.trt   <- tail(Q.trt, -1)

  df.vector.units <- dffun(M. = M.units, marg = marginality.unit)
  df.vector.trt   <- dffun(M. = M.trt,   marg = marginality.trt)

  df.vector.units <- tail(df.vector.units, -1)
  df.vector.trt   <- tail(df.vector.trt, -1)

  table.df <- tibble::tibble(
    Source.unit = character(),
    Source.trt  = character(),
    df.unit     = character(),
    df.trt      = character(),
    efficience   = character()
  )

  for (i in seq_along(Q.units)) {
    last_row1 <- nrow(table.df)
    table.df <- tibble::add_row(
      table.df,
      Source.unit = labels.unit1$term[i],
      Source.trt  = "",
      df.unit     = as.character(df.vector.units$df[i]),
      df.trt      = "",
      efficience   = "",
      .after      = last_row1
    )

    Q.unit <- Q.units[[i]]
    df <- 0
    added_any_trt <- FALSE

    for (j in seq_along(Q.trt)) {
      last_row2 <- nrow(table.df)
      Proj <- Q.unit %*% Q.trt[[j]] %*% Q.unit
      eigvals <- eigen(Proj)$values
      pos <- eigvals[eigvals > 1e-10]
      hm <- psych::harmonic.mean(pos)

      if (!is.na(hm) && hm != 0) {
        dfj <- length(pos)
        if (dfj > 0) {
          table.df <- tibble::add_row(
            table.df,
            Source.unit = "",
            Source.trt  = labels.trt1$term[j],
            df.unit     = "",
            df.trt      = as.character(dfj),
            efficience   = as.character(round(hm, 2)),
            .after      = last_row2
          )
          df <- df + dfj
          added_any_trt <- TRUE
        }
      }
    }

    dfres <- df.vector.units$df[i] - df
    if (added_any_trt && dfres > 0) {
      table.df <- tibble::add_row(
        table.df,
        Source.unit = "",
        Source.trt  = "Residual",
        df.unit     = "",
        df.trt      = as.character(dfres),
        efficience   = "",
        .after      = nrow(table.df)
      )
    }
  }

  knitr::kable(table.df)
}

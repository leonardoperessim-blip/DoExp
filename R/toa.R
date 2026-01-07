#' toa
#'
#' From the structural formulas of units and treatments and a data frame,
#' returns the Anatomy Table with degrees of freedom and the a-effiency value.
#'
#' @param units structural formula of units. Must be enclosed in quotation marks.
#' @param trt structural formula of treatments. Must be enclosed in quotation marks.
#' @param data dataframe containing the variables in the formulas.
#'
#' @example
#' a definir
#'

#'@export
toa <- function(units, trt, data) {

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
    efficence   = character()
  )

  for (i in seq_along(Q.units)) {
    last_row1 <- nrow(table.df)
    table.df <- tibble::add_row(
      table.df,
      Source.unit = labels.unit1$term[i],
      Source.trt  = "",
      df.unit     = as.character(df.vector.units$df[i]),
      df.trt      = "",
      efficence   = "",
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
            efficence   = as.character(round(hm)),
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
        efficence   = "",
        .after      = nrow(table.df)
      )
    }
  }

  knitr::kable(table.df)
}

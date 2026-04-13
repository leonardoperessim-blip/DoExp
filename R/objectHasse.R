#' objectHasse
#'
#' Returns the Hasse diagram with: source of variation, degrees of freedom,
#' matrices of the quadratic forms for sums of squares and the contributions to
#' the expectations of mean squares,from structural formulas.
#'
#' @param sform a structural formula as a character.
#' @param data data.frame containing the variables named in structural the
#'  formulas.
#' @param type indicates whether the factors are unit factors or treatment factors.
#' For unit factors: "unit", "units", "unrandomized", or "recipient".
#' For treatment factors: "treatment", "trt", "randomized", or "allocated".
#' @param random a vector containing the variables related to the random factors.
#' All factors for which the stated factors are marginal will be assumed to be
#' random.
#' @param advancedrandom a vector containing the variables related to the random.
#' factors. Here the factor for which the stated factors are marginal will not
#' be assumed to be random.
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
#'   units <- objectHasse(
#'     sform = "Blocks/Plots",
#'     data = blocks,
#'     type = "units",
#'     random = NULL,
#'     advancedrandom = NULL
#'   )
#'
#'   treatments <- objectHasse(
#'     sform = "A*C",
#'     data = blocks,
#'     type = "trt",
#'     random = c("A"),
#'     advancedrandom = NULL
#'   )
#' }

#'@export
objectHasse <- function(sform = NULL, data = NULL, type = NULL,
                         random = NULL, advancedrandom = NULL) {

  data <- datafactorfun(df = data)

  if (type %in% c("unit", "units", "recipient", "unrandomized")) {
    type <- "unit"
  } else if (type %in% c("treatment", "trt", "allocated", "randomized")) {
    type <- "trt"
  } else {
    stop("`type` must be: unit, units, recipient, unrandomized, treatment, trt, allocated or randomized.")
  }

  nn.sform <- nnfun(form = sform)

  X.sform <- Xfun(nn. = nn.sform, data = data)

  M.sform <- Mfun(X. = X.sform)

  marginality.sform <- marginalityfun(M.sform)

  hasse.sform <- hassefun(nn. = nn.sform, form = sform)

  arrow.sform <- arrowsfun(marginality.sform, hasse.sform)

  labels.sform <- parsetree5(sform)

  name.sform1 <- dplyr::mutate(
    hasse.sform,
    label_left_x  = .data$x - 1.5,
    label_right_x = .data$x + 1.5,
    label_y_left  = .data$y,
    label_y_right = .data$y,
    label_left  = stringr::str_replace_all(.data$name, ":", "^"),
    label_right = ifelse(
      .data$name == "Universe",
      "Mean",
      purrr::map_chr(.data$name, ~ matchfun(.x, labels.sform$term))
    )
  )

  name.sform2 <- dplyr::mutate(
    hasse.sform,
    label_left_x  = .data$x - 1.5,
    label_right_x = .data$x + 1.5,
    label_y_left  = .data$y + 4,
    label_y_right = .data$y + 4,
    label_left  = stringr::str_replace_all(.data$name, ":", "^"),
    label_right = ifelse(
      .data$name == "Universe",
      "Mean",
      purrr::map_chr(.data$name, ~ matchfun(.x, labels.sform$term))
    )
  )

  df.vector.sform <- dffun(M. = M.sform, marg = marginality.sform)

  M.vector.sform <- Mlabelfun(M. = M.sform, marg = marginality.sform, name. = name.sform1[-1, ])

  random_terms <- if (is.null(advancedrandom)) {
    if (type == "unit") c(random, as.character(tail(hasse.sform$name, 1))) else random
  } else {
    if (type == "unit") c(advancedrandom, as.character(tail(hasse.sform$name, 1))) else advancedrandom
  }

  if (is.null(advancedrandom)) {
    EMS.vector.sform <- emsfun(
      name.   = name.sform1,
      marg    = marginality.sform,
      random  = random_terms,
      M.      = M.sform
    )
  } else {
    EMS.vector.sform <- emsfun2(
      name.   = name.sform1,
      marg    = marginality.sform,
      random  = random_terms,
      M.      = M.sform
    )
  }

  df.vector.sform <- dplyr::mutate(
    hasse.sform,
    label_left_x  = .data$x - 2,
    label_right_x = .data$x + 2,
    label_y       = .data$y - 4,
    info_left     = df.vector.sform$levels,
    info_right    = df.vector.sform$df
  )

  M.vector.sform <- dplyr::mutate(
    hasse.sform,
    label_left_x  = .data$x - 2,
    label_right_x = .data$x + 2,
    label_y       = .data$y - 4,
    info_left     = M.vector.sform$M,
    info_right    = M.vector.sform$core
  )

  EMS.vector.sform <- dplyr::mutate(
    dplyr::filter(hasse.sform, .data$name != "Universe"),
    label_left_x  = .data$x - 2,
    label_right_x = .data$x + 2,
    label_y       = .data$y - 4,
    info_left     = EMS.vector.sform$effect[match(.data$name, EMS.vector.sform$term)],
    info_right    = EMS.vector.sform$EMS[match(.data$name, EMS.vector.sform$term)]
  )

  max_x.sform <- max(hasse.sform$x) + 20
  min_x.sform <- min(hasse.sform$x) - 20
  max_y.sform <- max(hasse.sform$y) + 10
  min_y.sform <- min(hasse.sform$y) - 20

  out <- list(
    plot_data = list(
      nodes = hasse.sform,
      arrows = arrow.sform,
      labels = name.sform2,
      df = df.vector.sform,
      M = M.vector.sform,
      EMS = EMS.vector.sform,
      limits = list(
        x = c(min_x.sform, max_x.sform),
        y = c(min_y.sform, max_y.sform)
      )
    ),
    raw = list(
      data = data,
      X = X.sform,
      marginality = marginality.sform
    )
  )

  class(out) <- "object_hasse"
  return(out)
}

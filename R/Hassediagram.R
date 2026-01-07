#' Hassediagram
#'
#' From the structural formulas of units and treatments and a data frame,
#' returns the Hasse diagrams with the quantities of interest: sources of
#' variation, degrees of freedom, core matrices of the quadratic forms for
#' sums of squares, and the contributions to the expectations of mean squares.
#'
#' @param units structural formula of units. Must be enclosed in quotation marks.
#' @param trt structural formula of treatments. Must be enclosed in quotation marks.
#' @param data dataframe containing the variables in the formulas.
#' @param random random-effect variables that extend randomness to the factors
#'  with respect to which they are marginal.
#' @param advancedrandom random-effect variables that do not extend randomness
#' to the factors with respect to which they are marginal.
#' @param leftcolor sets the color of everything to the left of the dot.
#' Default is "blue".
#' @param rightcolor sets the color of everything to the right of the dot.
#' Default is "red".
#'
#' @example
#' a definir
#'



#'@export
Hassediagram <- function(units = NULL, trt = NULL, data = NULL,
                         random = NULL, advancedrandom = NULL,
                         leftcolor = "blue", rightcolor = "red") {

  data <- datafactorfun(df = data)

  nn.units <- nnfun(form = units)
  nn.trt   <- nnfun(form = trt)

  X.units <- Xfun(nn. = nn.units, data = data)
  X.trt   <- Xfun(nn. = nn.trt,   data = data)

  M.units <- Mfun(X. = X.units)
  M.trt   <- Mfun(X. = X.trt)

  marginality.unit <- marginalityfun(M.units)
  marginality.trt  <- marginalityfun(M.trt)

  hasse.unit <- hassefun(nn. = nn.units, form = units)
  hasse.trt  <- hassefun(nn. = nn.trt,   form = trt)

  arrow.unit <- arrowsfun(marginality.unit, hasse.unit)
  arrow.trt  <- arrowsfun(marginality.trt,  hasse.trt)

  max_x.units <- max(hasse.unit$x) + 10
  min_x.units <- min(hasse.unit$x) - 10
  max_y.units <- max(hasse.unit$y) + 10
  min_y.units <- min(hasse.unit$y) - 10

  max_x.trt <- max(hasse.trt$x) + 10
  min_x.trt <- min(hasse.trt$x) - 10
  max_y.trt <- max(hasse.trt$y) + 10
  min_y.trt <- min(hasse.trt$y) - 10

  labels.unit <- parsetree5(units)
  labels.trt  <- parsetree5(trt)

  name.unit1 <- dplyr::mutate(
    hasse.unit,
    label_left_x  = .data$x - 1.5,
    label_right_x = .data$x + 1.5,
    label_y_left  = .data$y,
    label_y_right = .data$y,
    label_left  = stringr::str_replace_all(.data$name, ":", "^"),
    label_right = ifelse(
      .data$name == "Universe",
      "Mean",
      purrr::map_chr(.data$name, ~ matchfun(.x, labels.unit$term))
    )
  )

  name.trt1 <- dplyr::mutate(
    hasse.trt,
    label_left_x  = .data$x - 1.5,
    label_right_x = .data$x + 1.5,
    label_y_left  = .data$y,
    label_y_right = .data$y,
    label_left  = stringr::str_replace_all(.data$name, ":", "^"),
    label_right = ifelse(
      .data$name == "Universe",
      "Mean",
      purrr::map_chr(.data$name, ~ matchfun(.x, labels.trt$term))
    )
  )

  name.unit2 <- dplyr::mutate(
    hasse.unit,
    label_left_x  = .data$x - 1.5,
    label_right_x = .data$x + 1.5,
    label_y_left  = .data$y + 4,
    label_y_right = .data$y + 4,
    label_left  = stringr::str_replace_all(.data$name, ":", "^"),
    label_right = ifelse(
      .data$name == "Universe",
      "Mean",
      purrr::map_chr(.data$name, ~ matchfun(.x, labels.unit$term))
    )
  )

  name.trt2 <- dplyr::mutate(
    hasse.trt,
    label_left_x  = .data$x - 1.5,
    label_right_x = .data$x + 1.5,
    label_y_left  = .data$y + 4,
    label_y_right = .data$y + 4,
    label_left  = stringr::str_replace_all(.data$name, ":", "^"),
    label_right = ifelse(
      .data$name == "Universe",
      "Mean",
      purrr::map_chr(.data$name, ~ matchfun(.x, labels.trt$term))
    )
  )

  df.vector.units <- dffun(M. = M.units, marg = marginality.unit)
  df.vector.trt   <- dffun(M. = M.trt,   marg = marginality.trt)

  M.vector.units <- Mlabelfun(M. = M.units, marg = marginality.unit, name. = name.unit1[-1, ])
  M.vector.trt   <- Mlabelfun(M. = M.trt,   marg = marginality.trt,  name. = name.trt1[-1, ])

  if (is.null(advancedrandom)) {
    EMS.vector.units <- emsfun(
      name.   = name.unit1,
      marg    = marginality.unit,
      random  = random,
      M.      = M.units
    )
    EMS.vector.trt <- emsfun(
      name.   = name.trt2,
      marg    = marginality.trt,
      random  = random,
      M.      = M.trt
    )
  } else {
    EMS.vector.units <- emsfun2(
      name.   = name.unit1,
      marg    = marginality.unit,
      random  = advancedrandom,
      M.      = M.units
    )
    EMS.vector.trt <- emsfun2(
      name.   = name.trt2,
      marg    = marginality.trt,
      random  = advancedrandom,
      M.      = M.trt
    )
  }

  df.vector.units <- dplyr::mutate(
    hasse.unit,
    label_left_x  = .data$x - 2,
    label_right_x = .data$x + 2,
    label_y       = .data$y - 4,
    levels        = df.vector.units$levels,
    df            = df.vector.units$df
  )

  df.vector.trt <- dplyr::mutate(
    hasse.trt,
    label_left_x  = .data$x - 2,
    label_right_x = .data$x + 2,
    label_y       = .data$y - 4,
    levels        = df.vector.trt$levels,
    df            = df.vector.trt$df
  )

  M.vector.units <- dplyr::mutate(
    hasse.unit,
    label_left_x  = .data$x - 2,
    label_right_x = .data$x + 2,
    label_y       = .data$y - 4,
    M             = M.vector.units$M,
    core          = M.vector.units$core
  )

  M.vector.trt <- dplyr::mutate(
    hasse.trt,
    label_left_x  = .data$x - 2,
    label_right_x = .data$x + 2,
    label_y       = .data$y - 4,
    M             = M.vector.trt$M,
    core          = M.vector.trt$core
  )

  EMS.vector.units <- dplyr::mutate(
    dplyr::filter(hasse.unit, .data$name != "Universe"),
    label_left_x  = .data$x - 2,
    label_right_x = .data$x + 2,
    label_y       = .data$y - 4,
    effect        = EMS.vector.units$effect[match(.data$name, EMS.vector.units$term)],
    EMS           = EMS.vector.units$EMS[match(.data$name, EMS.vector.units$term)]
  )

  EMS.vector.trt <- dplyr::mutate(
    dplyr::filter(hasse.trt, .data$name != "Universe"),
    label_left_x  = .data$x - 2,
    label_right_x = .data$x + 2,
    label_y       = .data$y - 4,
    effect        = EMS.vector.trt$effect[match(.data$name, EMS.vector.trt$term)],
    EMS           = EMS.vector.trt$EMS[match(.data$name, EMS.vector.trt$term)]
  )

  factors.graph.unit <- ggplot2::ggplot(NULL, ggplot2::aes(x = c(0, 285), y = c(0, 650))) +
    ggplot2::geom_point(
      data = hasse.unit,
      ggplot2::aes(x = .data$x, y = .data$y),
      size = 4,
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = arrow.unit,
      ggplot2::aes(
        x = .data$xini,
        y = .data$yini + 1.5,
        xend = .data$xend,
        yend = .data$yend - 1.5
      ),
      arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "open"),
      color = "black",
      linewidth = 0.7
    ) +
    ggplot2::geom_text(
      data = name.unit1,
      ggplot2::aes(x = .data$label_left_x, y = .data$label_y_left, label = .data$label_left),
      hjust = 1,
      color = leftcolor
    ) +
    ggplot2::geom_text(
      data = name.unit1,
      ggplot2::aes(x = .data$label_right_x, y = .data$label_y_right, label = .data$label_right),
      hjust = 0,
      color = rightcolor
    ) +
    ggplot2::xlim(min_x.units, max_x.units) +
    ggplot2::ylim(min_y.units, max_y.units) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Units graph", subtitle = "Factors & Source of variation") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = ggplot2::element_blank()
    )

  factors.graph.trt <- ggplot2::ggplot(NULL, ggplot2::aes(x = c(0, 100), y = c(0, 400))) +
    ggplot2::geom_point(
      data = hasse.trt,
      ggplot2::aes(x = .data$x, y = .data$y),
      size = 4,
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = arrow.trt,
      ggplot2::aes(
        x = .data$xini,
        y = .data$yini + 1.5,
        xend = .data$xend,
        yend = .data$yend - 1.5
      ),
      arrow = grid::arrow(length = grid::unit(0.2, "cm")),
      color = "black",
      linewidth = 0.7
    ) +
    ggplot2::geom_text(
      data = name.trt1,
      ggplot2::aes(x = .data$label_left_x, y = .data$label_y_left, label = .data$label_left),
      hjust = 1,
      color = leftcolor
    ) +
    ggplot2::geom_text(
      data = name.trt1,
      ggplot2::aes(x = .data$label_right_x, y = .data$label_y_right, label = .data$label_right),
      hjust = 0,
      color = rightcolor
    ) +
    ggplot2::xlim(min_x.trt, max_x.trt) +
    ggplot2::ylim(min_y.trt, max_y.trt) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Treatments graph", subtitle = "Factors & Source of variation") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = ggplot2::element_blank()
    )

  df.graph.unit <- ggplot2::ggplot(NULL, ggplot2::aes(x = c(0, 285), y = c(0, 650))) +
    ggplot2::geom_point(
      data = hasse.unit,
      ggplot2::aes(x = .data$x, y = .data$y),
      size = 4,
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = arrow.unit,
      ggplot2::aes(
        x = .data$xini,
        y = .data$yini + 1.5,
        xend = .data$xend,
        yend = .data$yend - 1.5
      ),
      arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "open"),
      color = "black",
      linewidth = 0.7
    ) +
    ggplot2::geom_text(
      data = name.unit2,
      ggplot2::aes(x = .data$label_left_x, y = .data$label_y_left, label = .data$label_left),
      hjust = 1,
      color = leftcolor
    ) +
    ggplot2::geom_text(
      data = name.unit2,
      ggplot2::aes(x = .data$label_right_x, y = .data$label_y_right, label = .data$label_right),
      hjust = 0,
      color = rightcolor
    ) +
    ggplot2::geom_text(
      data = df.vector.units,
      ggplot2::aes(x = .data$label_left_x, y = .data$label_y, label = .data$levels),
      hjust = 1,
      color = leftcolor
    ) +
    ggplot2::geom_text(
      data = df.vector.units,
      ggplot2::aes(x = .data$label_right_x, y = .data$label_y, label = .data$df),
      hjust = 0,
      color = rightcolor
    ) +
    ggplot2::xlim(min_x.units, max_x.units) +
    ggplot2::ylim(min_y.units, max_y.units) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Units graph", subtitle = "Levels & Degrees of freedom") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = ggplot2::element_blank()
    )

  df.graph.trt <- ggplot2::ggplot(NULL, ggplot2::aes(x = c(0, 285), y = c(0, 650))) +
    ggplot2::geom_point(
      data = hasse.trt,
      ggplot2::aes(x = .data$x, y = .data$y),
      size = 4,
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = arrow.trt,
      ggplot2::aes(
        x = .data$xini,
        y = .data$yini + 1.5,
        xend = .data$xend,
        yend = .data$yend - 1.5
      ),
      arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "open"),
      color = "black",
      linewidth = 0.7
    ) +
    ggplot2::geom_text(
      data = name.trt2,
      ggplot2::aes(x = .data$label_left_x, y = .data$label_y_left, label = .data$label_left),
      hjust = 1,
      color = leftcolor
    ) +
    ggplot2::geom_text(
      data = name.trt2,
      ggplot2::aes(x = .data$label_right_x, y = .data$label_y_right, label = .data$label_right),
      hjust = 0,
      color = rightcolor
    ) +
    ggplot2::geom_text(
      data = df.vector.trt,
      ggplot2::aes(x = .data$label_left_x, y = .data$label_y, label = .data$levels),
      hjust = 1,
      color = leftcolor
    ) +
    ggplot2::geom_text(
      data = df.vector.trt,
      ggplot2::aes(x = .data$label_right_x, y = .data$label_y, label = .data$df),
      hjust = 0,
      color = rightcolor
    ) +
    ggplot2::xlim(min_x.trt, max_x.trt) +
    ggplot2::ylim(min_y.trt, max_y.trt) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Treatments graph", subtitle = "Levels & Degrees of freedom") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = ggplot2::element_blank()
    )

  M.graph.unit <- ggplot2::ggplot(NULL, ggplot2::aes(x = c(0, 285), y = c(0, 650))) +
    ggplot2::geom_point(
      data = hasse.unit,
      ggplot2::aes(x = .data$x, y = .data$y),
      size = 4,
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = arrow.unit,
      ggplot2::aes(
        x = .data$xini,
        y = .data$yini + 1.5,
        xend = .data$xend,
        yend = .data$yend - 1.5
      ),
      arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "open"),
      color = "black",
      linewidth = 0.7
    ) +
    ggplot2::geom_text(
      data = name.unit2,
      ggplot2::aes(x = .data$label_left_x, y = .data$label_y_left, label = .data$label_left),
      hjust = 1,
      color = leftcolor
    ) +
    ggplot2::geom_text(
      data = name.unit2,
      ggplot2::aes(x = .data$label_right_x, y = .data$label_y_right, label = .data$label_right),
      hjust = 0,
      color = rightcolor
    ) +
    ggplot2::geom_text(
      data = M.vector.units,
      ggplot2::aes(x = .data$label_left_x, y = .data$label_y, label = .data$M),
      hjust = 1,
      color = leftcolor,
      parse = TRUE
    ) +
    ggplot2::geom_text(
      data = M.vector.units,
      ggplot2::aes(x = .data$label_right_x, y = .data$label_y, label = .data$core),
      hjust = 0,
      color = rightcolor,
      parse = TRUE
    ) +
    ggplot2::xlim(min_x.units, max_x.units) +
    ggplot2::ylim(min_y.units, max_y.units) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Units graph", subtitle = "Projection matrices  & Core matrices") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = ggplot2::element_blank()
    )

  M.graph.trt <- ggplot2::ggplot(NULL, ggplot2::aes(x = c(0, 285), y = c(0, 650))) +
    ggplot2::geom_point(
      data = hasse.trt,
      ggplot2::aes(x = .data$x, y = .data$y),
      size = 4,
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = arrow.trt,
      ggplot2::aes(
        x = .data$xini,
        y = .data$yini + 1.5,
        xend = .data$xend,
        yend = .data$yend - 1.5
      ),
      arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "open"),
      color = "black",
      linewidth = 0.7
    ) +
    ggplot2::geom_text(
      data = name.trt2,
      ggplot2::aes(x = .data$label_left_x, y = .data$label_y_left, label = .data$label_left),
      hjust = 1,
      color = leftcolor
    ) +
    ggplot2::geom_text(
      data = name.trt2,
      ggplot2::aes(x = .data$label_right_x, y = .data$label_y_right, label = .data$label_right),
      hjust = 0,
      color = rightcolor
    ) +
    ggplot2::geom_text(
      data = M.vector.trt,
      ggplot2::aes(x = .data$label_left_x, y = .data$label_y, label = .data$M),
      hjust = 1,
      color = leftcolor,
      parse = TRUE
    ) +
    ggplot2::geom_text(
      data = M.vector.trt,
      ggplot2::aes(x = .data$label_right_x, y = .data$label_y, label = .data$core),
      hjust = 0,
      color = rightcolor,
      parse = TRUE
    ) +
    ggplot2::xlim(min_x.trt, max_x.trt) +
    ggplot2::ylim(min_y.trt, max_y.trt) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Treatments graph", subtitle = "Projection matrices & Core matrices") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = ggplot2::element_blank()
    )

  EMS.graph.unit <- ggplot2::ggplot(NULL, ggplot2::aes(x = c(0, 285), y = c(0, 650))) +
    ggplot2::geom_point(
      data = hasse.unit,
      ggplot2::aes(x = .data$x, y = .data$y),
      size = 4,
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = arrow.unit,
      ggplot2::aes(
        x = .data$xini,
        y = .data$yini + 1.5,
        xend = .data$xend,
        yend = .data$yend - 1.5
      ),
      arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "open"),
      color = "black",
      linewidth = 0.7
    ) +
    ggplot2::geom_text(
      data = name.unit2,
      ggplot2::aes(x = .data$label_left_x, y = .data$label_y_left, label = .data$label_left),
      hjust = 1,
      color = leftcolor
    ) +
    ggplot2::geom_text(
      data = name.unit2,
      ggplot2::aes(x = .data$label_right_x, y = .data$label_y_right, label = .data$label_right),
      hjust = 0,
      color = rightcolor
    ) +
    ggplot2::geom_text(
      data = EMS.vector.units,
      ggplot2::aes(x = .data$label_left_x, y = .data$label_y, label = .data$effect),
      hjust = 1,
      color = leftcolor,
      parse = TRUE
    ) +
    ggplot2::geom_text(
      data = EMS.vector.units,
      ggplot2::aes(x = .data$label_right_x, y = .data$label_y, label = .data$EMS),
      hjust = 0,
      color = rightcolor,
      parse = TRUE
    ) +
    ggplot2::xlim(min_x.units, max_x.units) +
    ggplot2::ylim(min_y.units, max_y.units) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Units graph", subtitle = "Variance component  & Expected mean square") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = ggplot2::element_blank()
    )

  EMS.graph.trt <- ggplot2::ggplot(NULL, ggplot2::aes(x = c(0, 285), y = c(0, 650))) +
    ggplot2::geom_point(
      data = hasse.trt,
      ggplot2::aes(x = .data$x, y = .data$y),
      size = 4,
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = arrow.trt,
      ggplot2::aes(
        x = .data$xini,
        y = .data$yini + 1.5,
        xend = .data$xend,
        yend = .data$yend - 1.5
      ),
      arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "open"),
      color = "black",
      linewidth = 0.7
    ) +
    ggplot2::geom_text(
      data = name.trt2,
      ggplot2::aes(x = .data$label_left_x, y = .data$label_y_left, label = .data$label_left),
      hjust = 1,
      color = leftcolor
    ) +
    ggplot2::geom_text(
      data = name.trt2,
      ggplot2::aes(x = .data$label_right_x, y = .data$label_y_right, label = .data$label_right),
      hjust = 0,
      color = rightcolor
    ) +
    ggplot2::geom_text(
      data = EMS.vector.trt,
      ggplot2::aes(x = .data$label_left_x, y = .data$label_y, label = .data$effect),
      hjust = 1,
      color = leftcolor,
      parse = TRUE
    ) +
    ggplot2::geom_text(
      data = EMS.vector.trt,
      ggplot2::aes(x = .data$label_right_x, y = .data$label_y, label = .data$EMS),
      hjust = 0,
      color = rightcolor,
      parse = TRUE
    ) +
    ggplot2::xlim(min_x.trt, max_x.trt) +
    ggplot2::ylim(min_y.trt, max_y.trt) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Treatments graph", subtitle = "Variance component  & Expected mean square") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = ggplot2::element_blank()
    )

  graph.list <- list(
    factors.graph.unit = factors.graph.unit,
    factors.graph.trt  = factors.graph.trt,
    df.graph.unit      = df.graph.unit,
    df.graph.trt       = df.graph.trt,
    M.graph.unit       = M.graph.unit,
    M.graph.trt        = M.graph.trt,
    EMS.graph.unit     = EMS.graph.unit,
    EMS.graph.trt      = EMS.graph.trt
  )

  return(graph.list)
}

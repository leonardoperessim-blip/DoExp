#' Hassediagramdf
#'
#' Returns the Hasse diagram with: source of variation, degrees of freedom,
#' matrices of the quadratic forms for sums of squares and the contributions to
#' the expectations of mean squares,from structural formulas.
#'
#' @param sform a structural formula as a character.
#' @param data data.frame containing the variables named in structural the
#'  formulas.
#' @param rightcolor the color of everything to the right of the dot in the
#' Hasse diagram. Default is "red".
#' @param titlehd The graph title. Can be NULL. Default: "Hasse diagram".
#' @param subtitlehd The graph subtitle. Can be NULL. Default: "Levels & Degrees of freedom".
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
#'   unitsdf <- Hassediagramdf(
#'     sform = "Blocks/Plots",
#'     data = blocks,
#'     rightcolor = "red",
#'     titlehd = "Hasse diagram",
#'     subtitlehd = "Levels & Degrees of freedom"
#'   )
#'
#'   trtdf <- Hassediagramdf(
#'     sform = "A*C",
#'     data = blocks,
#'     rightcolor = "blue",
#'     titlehd = "Hasse diagram",
#'     subtitlehd = "Levels & Degrees of freedom"
#'   )
#'
#'   unitsdf
#'   trtdf
#' }


#'@export
Hassediagramdf <- function(sform = NULL, data = NULL, rightcolor = "red",
                           titlehd = "Hasse diagram",
                           subtitlehd = "Levels & Degrees of freedom") {

  data <- datafactorfun(df = data)

  nn.sform <- nnfun(form = sform)

  X.sform <- Xfun(nn. = nn.sform, data = data)

  M.sform <- Mfun(X. = X.sform)

  marginality.sform <- marginalityfun(M.sform)

  hasse.sform <- hassefun(nn. = nn.sform, form = sform)

  arrow.sform <- arrowsfun(marginality.sform, hasse.sform)

  max_x.sform <- max(hasse.sform$x) + 10
  min_x.sform <- min(hasse.sform$x) - 10
  max_y.sform <- max(hasse.sform$y) + 10
  min_y.sform <- min(hasse.sform$y) - 10

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

  df.vector.sform <- dplyr::mutate(
    hasse.sform,
    label_left_x  = .data$x - 2,
    label_right_x = .data$x + 2,
    label_y       = .data$y - 4,
    levelsanddf   = paste0("[", df.vector.sform$levels, ",", df.vector.sform$df, "]")
  )

  df.graph.sform <- ggplot2::ggplot(NULL, ggplot2::aes(x = c(0, 285), y = c(0, 650))) +
    ggplot2::geom_point(
      data = hasse.sform,
      ggplot2::aes(x = .data$x, y = .data$y),
      size = 4,
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = arrow.sform,
      ggplot2::aes(
        x = .data$xini,
        y = .data$yini + 1.5,
        xend = .data$xend,
        yend = .data$yend - 1.5
      ),
      arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "open"),
      color = "black",
      linewidth = 0.7,
      alpha = 0.4
    ) +
  ggplot2::geom_text(
    data = name.sform2,
    ggplot2::aes(x = .data$label_right_x, y = .data$label_y_right, label = .data$label_right),
    hjust = 0,
    color = rightcolor
  ) +
  ggplot2::geom_text(
    data = df.vector.sform,
    ggplot2::aes(x = .data$label_right_x, y = .data$label_y, label = .data$levelsanddf),
    hjust = 0,
    color = rightcolor
  ) +
  ggplot2::xlim(min_x.sform, max_x.sform) +
  ggplot2::ylim(min_y.sform, max_y.sform) +
  ggplot2::theme_void() +
  ggplot2::labs(title = titlehd, subtitle = subtitlehd) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = ggplot2::element_blank()
  )

graph.list <- list(
  df.graph.sform      = df.graph.sform
)

return(graph.list)
}

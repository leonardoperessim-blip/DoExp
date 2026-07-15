#' plotHasse
#'
#' Returns the Hasse diagram with: source of variation, degrees of freedom,
#' matrices of the quadratic forms for sums of squares and the contributions to
#' the expectations of mean squares,from structural formulas.
#'
#' @param diagram an object from objectHasse with informations for ploting.
#' @param which a character to select which graph will be generated.
#' Choose between: "sv", "df", "M", and "EMS".
#' @param titlehd The graph title. Can be NULL. Default: "Hasse diagram".
#' @param subtitlehd The graph subtitle. Can be NULL. Default: NULL.
#' @param leftcolor the color of everything to the left of the dot in the Hasse
#' diagram. Default is "blue".
#' @param rightcolor the color of everything to the right of the dot in the
#' Hasse diagram. Default is "red".
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
#'
#'   svunits <- plotHasse(diagram = units, which = "sv")
#'   svtrt   <- plotHasse(diagram = treatments, which = "sv")
#'
#' svunits
#' svtrt
#' }




#'@export
plotHasse <- function(diagram = NULL, which = NULL,
                      titlehd = "Hasse diagram", subtitlehd = "",
                      leftcolor = "blue", rightcolor = "red") {
  if (!which %in% c("sv", "df", "M", "EMS")) {
    stop("'which' must be one of 'sv', 'df', 'M', or 'EMS'")
  }
  nodes <- diagram$plot_data$nodes
  arrows <- diagram$plot_data$arrows
  diagramsv <- diagram$plot_data$labels
  max_x <- diagram$plot_data$limits$x[2]
  min_x <- diagram$plot_data$limits$x[1]
  max_y <- diagram$plot_data$limits$y[2]
  min_y <- diagram$plot_data$limits$y[1]

  if(which == "sv"){
    graphout <- graph.sform <- ggplot2::ggplot(NULL, ggplot2::aes(x = c(0, 285), y = c(0, 650))) +
      ggplot2::geom_point(
        data = nodes,
        ggplot2::aes(x = .data$x, y = .data$y),
        size = 4,
        color = "black"
      ) +
      ggplot2::geom_segment(
        data = arrows,
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
        data = diagramsv,
        ggplot2::aes(x = .data$label_left_x, y = .data$label_y_left, label = .data$label_left),
        hjust = 1,
        color = leftcolor
      ) +
      ggplot2::geom_text(
        data = diagramsv,
        ggplot2::aes(x = .data$label_right_x, y = .data$label_y_right, label = .data$label_right),
        hjust = 0,
        color = rightcolor
      ) +
      ggplot2::xlim(min_x, max_x) +
      ggplot2::ylim(min_y, max_y) +
      ggplot2::theme_void() +
      ggplot2::labs(title = titlehd, subtitle = subtitlehd) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = ggplot2::element_blank()
      )
  }
  if(which == "df"){
    diagram <- diagram$plot_data$df
  }
  if(which == "M"){
    diagram <- diagram$plot_data$M
  }
  if(which == "EMS"){
    diagram <- diagram$plot_data$EMS
  }
  if(which != "sv"){
    graphout <- ggplot2::ggplot(NULL, ggplot2::aes(x = c(0, 285), y = c(0, 650))) +
    ggplot2::geom_point(
      data = nodes,
      ggplot2::aes(x = .data$x, y = .data$y),
      size = 4,
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = arrows,
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
        data = diagramsv,
        ggplot2::aes(x = .data$label_left_x, y = .data$label_y_left, label = .data$label_left),
        hjust = 1,
        color = leftcolor
      ) +
      ggplot2::geom_text(
        data = diagramsv,
        ggplot2::aes(x = .data$label_right_x, y = .data$label_y_right, label = .data$label_right),
        hjust = 0,
        color = rightcolor
      ) +
      ggplot2::geom_text(
        data = diagram,
        ggplot2::aes(x = .data$label_left_x, y = .data$label_y, label = .data$info_left),
        hjust = 1,
        color = leftcolor,
        parse = TRUE
      ) +
      ggplot2::geom_text(
        data = diagram,
        ggplot2::aes(x = .data$label_right_x, y = .data$label_y, label = .data$info_right),
        hjust = 0,
        color = rightcolor,
        parse = TRUE
      ) +

    ggplot2::xlim(min_x, max_x) +
    ggplot2::ylim(min_y, max_y) +
    ggplot2::theme_void() +
    ggplot2::labs(title = titlehd, subtitle = subtitlehd) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = ggplot2::element_blank()
    )}

  return(graphout)
}

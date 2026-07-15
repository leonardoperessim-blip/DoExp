arrowsfun <- function(marg, df) {
  arrows <- list()

  levels <- sort(unique(df$stratum), decreasing = TRUE)

  for (now_level in levels) {
    now_factor <- dplyr::filter(df, stratum == now_level)

    plus_level <- now_level - 1
    plus_factor <- dplyr::filter(df, stratum == plus_level)

    for (factor in now_factor$name) {
      for (fator_superior in plus_factor$name) {
        if (marg[factor, fator_superior] == 1) {
          coordinate_ini <- dplyr::filter(df, name == factor)
          coordinate_end <- dplyr::filter(df, name == fator_superior)

          arrows <- append(arrows, list(
            data.frame(
              xini = coordinate_ini$x,
              yini = coordinate_ini$y,
              xend = coordinate_end$x,
              yend = coordinate_end$y
            )
          ))
        }
      }
    }
  }

  arrows_df <- do.call(rbind, arrows)
  return(arrows_df)
}

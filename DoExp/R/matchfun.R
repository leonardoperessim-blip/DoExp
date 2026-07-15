matchfun <- function(label, labels_available) {
  letters <- sort(unlist(strsplit(label, ":")))

  for (n in labels_available) {
    letters_n <- sort(
      unique(stringr::str_extract_all(n, "[A-Za-z]+")[[1]])
    )
    if (identical(letters_n, letters)) {
      return(n)
    }
  }

  return(NA_character_)
}

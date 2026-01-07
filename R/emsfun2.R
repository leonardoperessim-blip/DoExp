emsfun2 <- function(name., marg, random, M.) {
  terms <- name.$name
  stopifnot(all(terms %in% rownames(marg)),
            all(terms %in% colnames(marg)))

  random_vec <- random
  subs <- name.$label_left

  is_rand <- terms %in% random_vec

  coefs <- vapply(terms, coefEMS, numeric(1), M. = M.)

  sigma_piece <- function(j) {
    base <- paste0('sigma["', subs[j], '"]^2')
    if (is.na(coefs[j])) base else paste0(sprintf("%.6g", coefs[j]), "*", base)
  }
  sigma_str <- vapply(seq_along(terms), sigma_piece, character(1))

  qpsi_str <- paste0('q["', subs, '"](psi)')

  effect <- ifelse(is_rand, sigma_str, qpsi_str)

  EMS <- vapply(seq_along(terms), function(i) {
    tname <- terms[i]
    idx_r <- which(is_rand & marg[terms, tname, drop = TRUE] == 1)
    if (length(idx_r) == 0) {
      if (is_rand[i]) "0" else qpsi_str[i]
    } else {
      S <- paste(sigma_str[idx_r], collapse = " + ")
      if (is_rand[i]) S else paste0(qpsi_str[i], " + ", S)
    }
  }, character(1))

  tibble::tibble(
    term   = terms,
    effect = effect,
    EMS    = EMS
  )
}

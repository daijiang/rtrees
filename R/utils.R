#' Captize the first letter of a word.
#' @param x A word.
#'
cap_first_letter = function (x) {
  sub("^([a-z])", "\\U\\1", x, perl = TRUE)
}
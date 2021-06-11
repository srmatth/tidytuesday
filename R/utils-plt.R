# This file will contain helper functions for graphing

#' Thousands Labeller
#' 
#' Take a number, divide it by a thousand and add a capital "K"
#'
#' @param x The number to be labeled
#'
#' @return A character string
#' @export
label_thousands <- function(x) {
  stringr::str_c(x / 1000, ifelse(x == 0, "", "K"))
}
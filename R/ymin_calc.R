#' Internal function to calculate y-values for Lasagna plots
#'
#' @param n vector of counts.
#'

ymin_calc <- function(n) {
  ymins <- dplyr::lag(cumsum(n))
  if (length(ymins) > 0) {
    ymins[1] <- 0
  }
  return(ymins)
}

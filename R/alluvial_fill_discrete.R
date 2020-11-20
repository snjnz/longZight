#' Alternative fill function for Alluvial objects
#'
#' Enables the use of a highlight variable in \code{\link{plot_long_alluvial}}
#' to enable easier reading of a standard Alluvial plot.
#'
#' Modifies output of the standard \code{discrete_scale} palette so levels that are not defined by \code{focus}
#' are made comparatively lighter.
#'
#' @param ...  Standard options to be passed on depending on scale type and
#' @param focus  The index of the factor that should be made lighter
#' @export
#'
alluvial_fill_discrete <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1, na.value = "grey50", aesthetics = "fill", focus = 1) {
  pal <- scales::hue_pal(h, c, l, h.start, direction)
  ggplot2::discrete_scale(aesthetics, "hue", (function(n) {
    orig.colours <- pal(n)
    lighter.colours <- colorspace::lighten(orig.colours, amount = 0.6, method = "relative")
    lighter.colours[focus] <- orig.colours[focus]
    return(lighter.colours)
  }), na.value = na.value, ...)
}

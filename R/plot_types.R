#' Helper function to return matrix of appropriate plot types for data frames.
#'
#' @param .data A longZight object generated by \code{\link[longZight:load_long_data]{load_long_data}}
#' @examples
#' \dontrun{
#' mydata %>%
#'   load_long_data(IDind, WAVE) %>%
#'   plot_types
#' }
#' @export

plot_types <- function(.data) {
  UseMethod("plot_types")
}

#' @rdname plot_types
#' @method plot_types longZight
#' @export
plot_types.longZight <- function(.data) {
  dts <- getDatatypes(.data) %>% tidyr::gather()

  id_var <- rlang::as_string(attr(.data, 'lZid'))
  wave_var <- rlang::as_string(attr(.data, 'lZwave'))

  wave_type <- dplyr::pull(dplyr::filter(dts, key == wave_var), value)

  tibble::add_column(dts,
                     alluvial = wave_type == "factor" & (dts$value == "factor" | dts$value == "character"),
                     fet = wave_type == "factor" & (dts$value == "factor" | dts$value == "character"),
                     stripline = dts$value == "numeric"
  ) %>% dplyr::filter(key != id_var, key != wave_var)
}

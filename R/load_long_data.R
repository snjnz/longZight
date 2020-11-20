#' Load data into longZight form
#'
#' @param .data A \code{\link[tibble:tbl_df-class]{tbl_df}} or
#'  \code{\link[base:data.frame]{data.frame}} object storing the data
#' @param id_col  Name, or \code{\link[rlang:sym]{sym}} object for the individual identifier column
#' @param time_col  Name, or \code{\link[rlang:sym]{sym}} object for the time/wave column
#'
#' @return \code{longZight} object with individual identifer and time information encoded
#' @examples
#' \dontrun{
#' mydata %>%
#'   load_long_data(IDind, WAVE)
#' }
#' @export

load_long_data <- function(.data, id_col = "id", time_col = "time", ts.args = TRUE) {
  if (inherits(.data, "data.frame", which = TRUE) == 1)
    .data = tibble::as_tibble(.data)
  else if (inherits(.data, "tbl_df", which = TRUE) != 1)
    stop(".data must be a tibble or data.frame object")

  if (ts.args) {
  id_col <- rlang::ensym(id_col)
  time_col <- rlang::ensym(time_col)
  } else {
    id_col <- rlang::sym(id_col)
    time_col <- rlang::sym(time_col)
  }
  if (!all(c(id_col, time_col) %in% colnames(.data))) {
    warning(paste0("id: ", id_col, "; time: ", time_col, "dim(.data): ", dim(.data)))
    stop("id_col and time_col must be present in .data")
  }

  class(.data) <- c("longZight", class(.data))
  attr(.data, 'lZwave') <- time_col
  attr(.data, 'lZid') <- id_col

  return(.data)
}

#' @export

load_long_data_s <- function(.data, id_col = "id", time_col = "time", ts.args = TRUE) {
  if (inherits(.data, "data.frame", which = TRUE) == 1)
    .data = tibble::as_tibble(.data)
  else if (inherits(.data, "tbl_df", which = TRUE) != 1)
    stop(".data must be a tibble or data.frame object")

  id_col <- rlang::sym(id_col)
  time_col <- rlang::sym(time_col)
  if (!all(c(id_col, time_col) %in% colnames(.data))) {
    warning(paste0("id: ", id_col, "; time: ", time_col, "dim(.data): ", dim(.data)))
    stop("id_col and time_col must be present in .data")
  }

  class(.data) <- c("longZight", class(.data))
  attr(.data, 'lZwave') <- time_col
  attr(.data, 'lZid') <- id_col

  return(.data)
}

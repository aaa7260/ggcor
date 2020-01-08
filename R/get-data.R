#' Helper function to extract cor_tbl.
#' @description These functions are used to quickly obtain the upper
#'     trig, lower trig, diagonal, or remove the diagonal of the correlation
#'     coefficient matrix.
#' @param x a cor_tbl object.
#' @param show.diag a logical value indicating whether keep the diagonal.
#' @return a modified cor_tbl object.
#' @importFrom dplyr filter
#' @rdname extract_cor_tbl
#' @examples
#' df <- fortify_cor(mtcars)
#' quickcor(df) + geom_colour()
#' df01 <- get_lower_data(df)
#' quickcor(df01) + geom_colour()
#' df02 <- get_upper_data(df, show.diag = FALSE)
#' quickcor(df02) + geom_colour()
#' df03 <- get_diag_data(df)
#' quickcor(df03) + geom_colour()
#' df04 <- get_diag_tri(df)
#' quickcor(df04) + geom_colour()
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
get_lower_data <- function(x, show.diag = TRUE)
{
  stopifnot(is_cor_tbl(x))
  if(!is_symmet(x)) {
    warning("Just supports symmetric correlation matrix.", call. = FALSE)
    return(x)
  }
  n <- length(get_col_name(x))
  if(isTRUE(show.diag)) {
    x <- with(x, subset(x, .row.id + .col.id <= n + 1))
  } else {
    x <- with(x, subset(x, .row.id + .col.id < n + 1))
  }
  attr(x, "type") <- "lower"
  attr(x, "show.diag") <- show.diag
  x
}
#' @rdname extract_cor_tbl
#' @export
get_upper_data <- function(x, show.diag = TRUE)
{
  stopifnot(is_cor_tbl(x))
  if(!is_symmet(x)) {
    warning("Just supports symmetric correlation matrix.", call. = FALSE)
    return(x)
  }
  n <- length(get_col_name(x))
  if(isTRUE(show.diag)) {
    x <- with(x, subset(x, .row.id + .col.id >= n + 1))
  } else {
    x <- with(x, subset(x, .row.id + .col.id > n + 1))
  }
  attr(x, "type") <- "upper"
  attr(x, "show.diag") <- show.diag
  x
}
#' @rdname extract_cor_tbl
#' @export
get_diag_tri <- function(x)
{
  stopifnot(is_cor_tbl(x))
  if(!is_symmet(x)) {
    warning("Just supports symmetric correlation matrix.", call. = FALSE)
    return(x)
  }
  n <- length(get_col_name(x))
  x <- with(x, subset(x, .row.id + .col.id != n + 1))
  if(get_type(x) %in% c("upper", "lower"))
    attr(x, "show.diag") <- FALSE
  x
}
#' @rdname extract_cor_tbl
#' @export
get_diag_data <- function(x)
{
  stopifnot(is_cor_tbl(x))
  if(!is_symmet(x)) {
    warning("Just supports symmetric correlation matrix.", call. = FALSE)
    return(x)
  }
  n <- length(get_col_name(x))
  with(x, subset(x, .row.id + .col.id == n + 1))
}

#' Create cor_tbl extractor function
#' @description This function returns another function that can extract cor_tbl
#'     subset from a cor_tbl object.
#' @param type a string, "full" (default), "upper" or "lower", display full,
#'     lower triangular or upper triangular matrix.
#' @param show.diag a logical value indicating whether keep the diagonal.
#' @param ... extra filter params, see Details.
#' @details This function is mainly used in \code{ggplot2} geom_*() functions,
#'     where data is filtered based on the \code{...} parameter, then subsets
#'     are extracted based on the type and show.diag parameters.
#' @return extractor function
#' @importFrom dplyr filter
#' @rdname get_data
#' @examples
#' quickcor(mtcars) +
#'   geom_square(data = get_data(type = "upper")) +
#'   geom_circle2(data = get_data(type = "lower", show.diag = FALSE))
#' @seealso \code{\link[dplyr]{filter}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
get_data <- function(..., type = "full", show.diag = FALSE)
{
  type <- match.arg(type, c("full", "upper", "lower", "diag"))
  function(data) {
    x <- dplyr::filter(data, ...)
    switch (type,
            full  = if(isTRUE(show.diag)) x else get_diag_tri(x),
            upper = get_upper_data(x, show.diag = show.diag),
            lower = get_lower_data(x, show.diag = show.diag),
            diag  = get_diag_data(x)
    )
  }
}

#' @noRd
is_symmet <- function(x) {
  stopifnot(is_cor_tbl(x))
  col.name <- get_col_name(x)
  row.name <- get_row_name(x)
  if((length(col.name) != length(row.name)) || !all(col.name == row.name)) {
    return(FALSE)
  }
  TRUE
}

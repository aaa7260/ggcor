#' @export
get_lower_data <- function(x, show.diag = TRUE)
{
  stopifnot(is_cor_tbl(x))
  if(!is_symmet(x)) {
    warning("Just supports symmetric correlation matrix.", call. = FALSE)
    return(x)
  }
  n <- length(cor_tbl_xname(x))
  if(isTRUE(show.diag)) {
    x <- dplyr::filter(x, x + y <= n + 1)
  } else {
    x <- dplyr::filter(x, x + y < n + 1)
  }
  attr(x, "type") <- "lower"
  attr(x, "show.diag") <- show.diag
  x
}
#' @export
get_upper_data <- function(x, show.diag = TRUE)
{
  stopifnot(is_cor_tbl(x))
  if(!is_symmet(x)) {
    warning("Just supports symmetric correlation matrix.", call. = FALSE)
    return(x)
  }
  n <- length(cor_tbl_xname(x))
  if(isTRUE(show.diag)) {
    x <- dplyr::filter(x, x + y >= n + 1)
  } else {
    x <- dplyr::filter(x, x + y > n + 1)
  }
  attr(x, "type") <- "upper"
  attr(x, "show.diag") <- show.diag
  x
}

#' @export
get_diag_tri <- function(x)
{
  stopifnot(is_cor_tbl(x))
  if(!is_symmet(x)) {
    warning("Just supports symmetric correlation matrix.", call. = FALSE)
    return(x)
  }
  n <- length(cor_tbl_xname(x))
  x <- dplyr::filter(x, x + y != n + 1)
  if(cor_tbl_type(x) %in% c("upper", "lower"))
    attr(x, "show.diag") <- FALSE
  x
}

#' @export
get_diag_data <- function(x)
{
  stopifnot(is_cor_tbl(x))
  if(!is_symmet(x)) {
    warning("Just supports symmetric correlation matrix.", call. = FALSE)
    return(x)
  }
  n <- length(cor_tbl_xname(x))
  dplyr::filter(x, xx + yy == n + 1)
}

#' @export
get_data <- function(..., type = "full", show.diag = TRUE)
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
  xname <- cor_tbl_xname(x)
  yname <- cor_tbl_yname(x)
  if((length(xname) != length(yname)) || !all(xname == rev(yname))) {
    return(FALSE)
  }
  TRUE
}

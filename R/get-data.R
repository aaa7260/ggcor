#' @export
get_lower_data <- function(x, show.diag = TRUE)
{
  if(!inherits(x, "cor_tbl"))
    x <- as_cor_tbl(x)
  if(!is_symmet(x)) {
    stop("Just supports symmetric correlation matrix.", call. = FALSE)
  }
  if(inherits(x, "cor_tbl_name")) {
    xx <- as.integer(x$x)
    yy = as.integer(x$y)
  } else {
    xx <- x$x
    yy <- x$y
  }
  n <- length(cor_tbl_xname(x))
  if(show.diag) {
    out <- dplyr::filter(x, xx + yy <= n + 1)
  } else {
    out <- dplyr::filter(x, xx + yy < n + 1)
  }
  attr(out, "type") <- "lower"
  attr(out, "show.diag") <- show.diag
  out
}
#' @export
get_upper_data <- function(x, show.diag = TRUE)
{
  if(!inherits(x, "cor_tbl"))
    x <- as_cor_tbl(x)
  if(!is_symmet(x)) {
    stop("Just supports symmetric correlation matrix.", call. = FALSE)
  }
  if(inherits(x, "cor_tbl_name")) {
    xx <- as.integer(x$x)
    yy = as.integer(x$y)
  } else {
    xx <- x$x
    yy <- x$y
  }
  n <- length(cor_tbl_xname(x))
  if(show.diag) {
    out <- dplyr::filter(x, xx + yy >= n + 1)
  } else {
    out <- dplyr::filter(x, xx + yy > n + 1)
  }
  attr(out, "type") <- "upper"
  attr(out, "show.diag") <- show.diag
  out
}

#' @export
get_diag_tri <- function(x)
{
  if(!inherits(x, "cor_tbl"))
    x <- as_cor_tbl(x)
  if(!is_symmet(x)) {
    stop("Just supports symmetric correlation matrix.", call. = FALSE)
  }
  if(inherits(x, "cor_tbl_name")) {
    xx <- as.integer(x$x)
    yy = as.integer(x$y)
  } else {
    xx <- x$x
    yy <- x$y
  }
  n <- length(cor_tbl_xname(x))
  out <- dplyr::filter(x, xx + yy != n + 1)
  if(cor_tbl_type(out) %in% c("upper", "lower"))
    attr(out, "show.diag") <- FALSE
  out
}

#' @export
get_diag_data <- function(x)
{
  if(!inherits(x, "cor_tbl"))
    x <- as_cor_tbl(x)
  if(!is_symmet(x)) {
    stop("Just supports symmetric correlation matrix.", call. = FALSE)
  }
  if(inherits(x, "cor_tbl_name")) {
    xx <- as.integer(x$x)
    yy = as.integer(x$y)
  } else {
    xx <- x$x
    yy <- x$y
  }
  n <- length(cor_tbl_xname(x))
  out <- dplyr::filter(x, xx + yy == n + 1)
  out
}

#' @export
get_data <- function(..., type = "full", show.diag = TRUE)
{
  function(data) {
    x <- dplyr::filter(data, ...)
    switch (type,
            full  = if(show.diag) x else get_diag_tri(x),
            upper = get_upper_data(x, show.diag = show.diag),
            lower = get_lower_data(x, show.diag = show.diag),
            diag  = get_diag_data(x)
    )
  }
}

#' @noRd
get_grid_data <- function(x) {
  if(!inherits(x, "cor_tbl"))
    stop("Need a cor_tbl.", call. = FALSE)
  n <- length(cor_tbl_xname(x))
  m <- length(cor_tbl_yname(x))
  type <- cor_tbl_type(x)
  show.diag <- cor_tbl_showdiag(x)
  dd <- expand.grid(1:n, 1:m)
  names(dd) <- c("x", "y")
  if(type == "full") {
    dd <- dd
  } else if(type == "upper") {
    idx <- if(show.diag) dd$x + dd$y >= n + 1 else dd$x + dd$y > n + 1
    dd <- dd[idx, ]
  } else {
    idx <- if(show.diag) dd$x + dd$y <= n + 1 else dd$x + dd$y < n + 1
    dd <- dd[idx, ]
  }
  dd
}

#' @noRd
is_symmet <- function(x) {
  stopifnot(inherits(x, "cor_tbl"))
  xname <- cor_tbl_xname(x)
  yname <- cor_tbl_yname(x)
  if((length(xname) != length(yname)) || !all(xname == rev(yname))) {
    return(FALSE)
  }
  TRUE
}


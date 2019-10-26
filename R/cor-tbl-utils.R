#' @export
cor_tbl_xname <- function(x) {
  stopifnot(inherits(x, "cor_tbl"))
  attr(x, "xname")
}
#' @export
cor_tbl_yname <- function(x) {
  stopifnot(inherits(x, "cor_tbl"))
  attr(x, "yname")
}
#' @export
cor_tbl_type <- function(x) {
  stopifnot(inherits(x, "cor_tbl"))
  attr(x, "type")
}
#' @export
cor_tbl_showdiag <- function(x) {
  stopifnot(inherits(x, "cor_tbl"))
  attr(x, "show.diag")
}

#' @export
cor_tbl_namebind <- function(x, add = FALSE, var_xname = NULL, var_yname = NULL)
{
  if(!inherits(x, "cor_tbl"))
    x <- as_cor_tbl(x)
  xname <- cor_tbl_xname(x)
  yname <- cor_tbl_yname(x)
  if(add) {
    if(is.null(var_xname))
      var_xname <- "xname"
    if(is.null(var_yname))
      var_yname <- "yname"
    if(var_xname %in% names(df)) {
      warning(var_xname, " has existed in 'x', will be replaced.", call. = FALSE)
      x[[var_xname]] <- xname[x$x]
    }
    if(var_yname %in% names(df)) {
      warning(var_yname, " has existed in 'x', will be replaced.", call. = FALSE)
      x[[var_yname]] <- yname[x$y]
    }
  } else {
    x$x <- xname[x$x]
    x$y <- yname[x$y]
  }
  x
}

#' @noRd
cor_tbl_check <- function(x) {
  stopifnot(inherits(x, c("data.frame", "cor_tbl")))
  name <- names(x)
  if(!all(c("x", "y", "r") %in% name)) {
    stop("The columns of `x` must contain 'x', 'y' and 'r'.", call. = FALSE)
  }
  if(!all(c("xname", "yname", "type") %in% names(attributes(x)))) {
    stop("The attributes of `x` must contain 'xname', 'yname' and 'type'.", call. = FALSE)
  }
  xname <- cor_tbl_xname(x)
  yname <- cor_tbl_yname(x)
  type <- cor_tbl_type(x)
  if(!type %in% c("full", "upper", "lower") || is.null(type)) {
    stop("'type' attributes must be 'full', 'upper' or 'lower'.", call. = FALSE)
  }
  if(!all(unique(x$x) %in% 1:length(xname))) {
    stop("The values of 'x' column in `x` is invalid.", call. = FALSE)
  }
  if(!all(unique(x$y) %in% 1:length(yname))) {
    stop("The values of 'y' column in `x` is invalid.", call. = FALSE)
  }
  if(!all(appro_gq(x$r, -1) & appro_lq(x$r, 1))) {
    stop("The values of 'r' column in `x` is invalid.", call. = FALSE)
  }
  if("p" %in% name) {
    if(!all(appro_gq(x$p, 0) & appro_lq(x$p, 1)))
      stop("The values of 'p' column in `x` is invalid.", call. = FALSE)
  }
  if("low" %in% name) {
    if(!all(appro_gq(x$low, - 1) & appro_lq(x$low, x$r)))
      stop("The values of 'low' column in `x` is invalid.", call. = FALSE)
  }
  if("upp" %in% name) {
    if(!all(appro_gq(x$upp, x$r) & appro_lq(x$upp, 1))) # handle float number
      stop("The values of 'upp' column in `x` is invalid.", call. = FALSE)
  }
}

#' @export
print.cor_tbl <- function(x, nmax = 5, width = 60) {
  tibble:::print.tbl(x, n = nmax, width = width)
  cat("Extra attributes:\n")
  cat_line(cor_tbl_xname(x), name = "xname:", width = width)
  cat_line(cor_tbl_yname(x), name = "yname:", width = width)
  cat_line(cor_tbl_showdiag(x), name = "show.diag:", width = width)
}

#' @export
plot.cor_tbl <- function(x, ...) {
  ggcor(x) + geom_raster(...)
}

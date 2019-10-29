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
cor_tbl_namebind <- function(x, add = FALSE, var_xname = NULL, var_yname = NULL,
                             keep.factor = TRUE)
{
  if(!inherits(x, "cor_tbl"))
    x <- as_cor_tbl(x)
  if(!inherits(x, "cor_tbl_name")) {
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
      x$x <- factor(xname[x$x], levels = xname)
      x$y <- factor(yname[x$y], levels = yname)
    }
  }
  if(!keep.factor) {
    x$x <- as.character(x$x)
    x$y <- as.character(x$y)
  }
  x
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
  ggcor(x, ...) + geom_raster()
}

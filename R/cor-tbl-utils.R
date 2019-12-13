#' Get attributes of cor_tbl
#' @description Helper function to get extra attributs of cor_tbl.
#' @param x a cor_tbl.
#' @return return attribute vector.
#' @rdname cor-tbl-attr
#' @export
#' @examples
#' df <- fortify_cor(mtcars)
#' ## get xname
#' cor_tbl_xname(df)
#' ## get yname
#' cor_tbl_yname(df)
#' ## get show.diag
#' cor_tbl_showdiag(df)
#' ## get type
#' cor_tbl_type(df)
#' @author Houyun Huang
#' @author Lei Zhou
#' @author Jian Chen
#' @author Taiyun Wei
#' @export
cor_tbl_xname <- function(x) {
  stopifnot(is_cor_tbl(x) || is_cor_tbl_fct(x))
  attr(x, "xname")
}
#' @rdname cor-tbl-attr
#' @export
cor_tbl_yname <- function(x) {
  stopifnot(is_cor_tbl(x) || is_cor_tbl_fct(x))
  attr(x, "yname")
}
#' @rdname cor-tbl-attr
#' @export
cor_tbl_type <- function(x) {
  stopifnot(is_cor_tbl(x))
  attr(x, "type")
}
#' @rdname cor-tbl-attr
#' @export
cor_tbl_showdiag <- function(x) {
  stopifnot(is_cor_tbl(x) || is_cor_tbl_fct(x))
  attr(x, "show.diag")
}
#' @rdname cor-tbl-attr
#' @export
is_cor_tbl <- function(x) {
  inherits(x, "cor_tbl")
}

#' Coerce cor_tbl to tbl_df
#' @param x cor_tbl object
#' @param rm.extra.attr a logical value indicating whether remove the extra attributes.
#' @return a tbl_df object.
#' @examples
#' df <- fortify_cor(mtcars)
#' cor_tbl2tbl_df(df)
#' @export
cor_tbl2tbl_df <- function(x, rm.extra.attr = TRUE) {
  stopifnot(is_cor_tbl(x))
  class(x) <- setdiff(class(x), "cor_tbl")
  if(rm.extra.attr) {
    attr(x, c("xname", "yname", "type", "show.diag")) <- NULL
  }
  x
}
#' Print a cor_tbl object
#' @param x A cor_tbl object.
#' @param n The max lines will be print (the default is 5).
#' @param width The max width for each line.
#' @param ... Extra params passing to `print()` function.
#' @export
#' @method print cor_tbl
#' @author Houyun Huang
print.cor_tbl <- function(x, n = 5, width = 60, ...) {
  tibble:::print.tbl(x, n = n, width = width, ...)
  cat("Extra attributes:\n")
  cat_line(cor_tbl_xname(x), name = "xname:", width = width)
  cat_line(cor_tbl_yname(x), name = "yname:", width = width)
  cat_line(cor_tbl_type(x), name = "type:", width = width)
  cat_line(cor_tbl_showdiag(x), name = "show.diag:", width = width)
}

#' Draw a cor_tbl object
#' @param x A cor_tbl object.
#' @param ... Extra params passing to `ggcor()` function.
#' @export
#' @method plot cor_tbl
#' @author Houyun Huang
plot.cor_tbl <- function(x, ...) {
  ggcor(x, ...) + geom_raster()
}


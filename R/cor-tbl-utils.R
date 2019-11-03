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
#' @export
cor_tbl_xname <- function(x) {
  stopifnot(inherits(x, "cor_tbl"))
  attr(x, "xname")
}
#' @rdname cor-tbl-attr
#' @export
cor_tbl_yname <- function(x) {
  stopifnot(inherits(x, "cor_tbl"))
  attr(x, "yname")
}
#' @rdname cor-tbl-attr
#' @export
cor_tbl_type <- function(x) {
  stopifnot(inherits(x, "cor_tbl"))
  attr(x, "type")
}
#' @rdname cor-tbl-attr
#' @export
cor_tbl_showdiag <- function(x) {
  stopifnot(inherits(x, "cor_tbl"))
  attr(x, "show.diag")
}
#' Convert cor_tbl from order to name
#' @description The cor_tbl x corresponds to the column of the correlation matrix,
#'     and y corresponds to the row of the correlation matrix. By default, x/y is
#'     the order in which the rows or columns appear, and this function binds x/y
#'     to the columns/rows name of the correlation matrix.
#' @param x A cor_tbl object.
#' @param add A logical value indicating whether binds x/y name to a new columns.
#' @param var_xname If add = TRUE, specifies the name of the new x column.
#' @param var_yname If add = TRUE, specifies the name of the new y column.
#' @param keep.factor A logical value indicating whether trans x/y name column to
#'     character vector.
#' @return A modified tbl.
#' @export
#' @author Houyun Huang
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
    class(x) <- c("cor_tbl_name", class(x))
  }
  if(!keep.factor) {
    if(add) {
      x[[var_xname]] <- as.character(x[[var_xname]])
      x[[var_yname]] <- as.character(x[[var_yname]])
    } else {
      x$x <- as.character(x$x)
      x$y <- as.character(x$y)
    }
    class(x) <- class(x)[!class(x) %in% c("cor_tbl", "col_tbl_name")]
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

#' Helper function of cor_tbl
#' @param x a cor_tbl.
#' @return return attribute value.
#' @rdname cor_tbl_attr
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
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
cor_tbl_xname <- function(x) {
  stopifnot(is_cor_tbl(x))
  attr(x, "xname")
}
#' @rdname cor_tbl_attr
#' @export
cor_tbl_yname <- function(x) {
  stopifnot(is_cor_tbl(x))
  attr(x, "yname")
}
#' @rdname cor_tbl_attr
#' @export
cor_tbl_type <- function(x) {
  stopifnot(is_cor_tbl(x))
  attr(x, "type")
}
#' @rdname cor_tbl_attr
#' @export
cor_tbl_showdiag <- function(x) {
  stopifnot(is_cor_tbl(x))
  attr(x, "show.diag")
}
#' @rdname cor_tbl_attr
#' @export
is_cor_tbl <- function(x) {
  inherits(x, "cor_tbl")
}







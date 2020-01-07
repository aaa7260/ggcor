
#' @importFrom dplyr mutate
#' @export
mutate.col_tbl <- function(.data, ...)
{
  attrs <- attributes(.data)
  .data <- mutate(as_tibble(.data), ...)
  set_attrs(.data, attrs)
}
#' @importFrom tibble as_tibble
#' @export
as_tibble.cor_tbl <- function(.data) 
{
  class(.data) <- setdiff(class(.data), "cor_tbl")
  remove_attrs(.data)
}
#' @importFrom dplyr bind_rows
#' @noRd
bind_cor_tbl_rows <- function(..., keep.attrs = c("first", "last"))
{
  keep.attrs <- match.arg(keep.attrs)
  cor.tbls <- list(...)
  stopifnot(length(cor.tbls) > 0)
  attrs <- if(keep.attrs == "first") {
    attributes(cor.tbls[[1]])
  }  else {
    attributes(cor.tbls[[length(cor.tbls)]])
  }
  .data <- dplyr::bind_rows(cor.tbls)
  set_attrs(.data, attrs)
}
#' @noRd
set_attrs <- function(.data, .attrs = list(), .excludes = NULL) 
{
  .excludes <- .excludes %||% c("names", "class", "row.names")
  new.attrs <- .attrs[setdiff(names(attrs), .excludes)]
  if(length(new.attrs) > 0) {
    for (nm in names(new.attrs)) {
      attr(.data, nm) <- new.attrs[[nm]]
    }
  }
  .data
}
#' @noRd
remove_attrs <- function(.data, .excludes = NULL) 
{
  attrs <- attributes(.data)
  .excludes <- .excludes %||% c("names", "class", "row.names")
  rm.attr.name <- setdiff(names(attrs), .excludes)
  if(length(rm.attr.name) > 0) {
    for (nm in rm.attr.name) {
      attr(.data, nm) <- NULL
    }
  }
  .data
}
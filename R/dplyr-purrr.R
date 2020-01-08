#' @importFrom tibble as_tibble
#' @export
as_tibble.cor_tbl <- function(.data, ...) 
{
  class(.data) <- setdiff(class(.data), "cor_tbl")
  attrs <- attributes(.data)
  excludes <- attrs[setdiff(names(attrs), c("names", "class", "row.names"))]
  if(length(excludes) > 0) {
    for (nm in names(excludes)) {
      attr(.data, nm) <- NULL
    }
  }
  .data
}

#' @importFrom dplyr mutate
#' @export
mutate.cor_tbl <- function(.data, ...)
{
  attrs <- attributes(.data)
  .data <- dplyr::mutate(as_tibble(.data), ...)
  set_attrs(.data, attrs)
}

#' @export
dplyr::`%>%`

#' @export
dplyr::mutate

#' @export
tibble::as_tibble

#' @noRd
set_attrs <- function(.data, .attrs = list(), .excludes = NULL) 
{
  .excludes <- .excludes %||% c("names", "class", "row.names")
  new.attrs <- .attrs[setdiff(names(.attrs), .excludes)]
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
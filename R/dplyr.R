#' @importFrom tibble as_tibble
#' @export
as_tibble.cor_tbl <- function(x, ...)
{
  class(x) <- setdiff(class(x), "cor_tbl")
  attrs <- attributes(x)
  excludes <- attrs[setdiff(names(attrs), c("names", "class", "row.names"))]
  if(length(excludes) > 0) {
    for (nm in names(excludes)) {
      attr(x, nm) <- NULL
    }
  }
  x
}

#' @importFrom dplyr mutate
#' @export
mutate.cor_tbl <- function(.data, ...)
{
  attrs <- attributes(.data)
  .data <- dplyr::mutate(as_tibble(.data), ...)
  set_attrs(.data, attrs, .excludes = c("names", "row.names"))
}

#' @importFrom dplyr mutate
#' @export
mutate.cor_network <- function(.data, active = NULL, ...)
{
  ll <- list(...)
  active <- active %||% attr(.data, "active") %||% "nodes"
  if(active == "nodes") {
    if("name" %in% names(ll))
      stop("'name' variable are preserved.", call. = FALSE)
    .data$nodes <- dplyr::mutate(.data$nodes, ...)
  } else {
    if(any(c("from", "to") %in% names(ll)))
      stop("'from', 'to' variables are preserved.", call. = FALSE)
    .data$edges <- dplyr::mutate(.data$edges, ...)
  }
  attr(.data, "active") <- active
  .data
}

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

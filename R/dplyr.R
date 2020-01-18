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
    if(any(c(".row.names", ".col.names") %in% names(ll)))
      stop("'.row.names', '.col.names' variables are preserved.", call. = FALSE)
    .data$edges <- dplyr::mutate(.data$edges, ...)
  }
  attr(.data, "active") <- active
  .data
}

#' @importFrom dplyr mutate
#' @export
mutate.grouped_cor_network <- function(.data, ...)
{
  lapply(attr(.data, "grouped_cn"), mutate, ...)
}

#' @importFrom dplyr group_by
#' @export
group_by.cor_network <- function(.data, ..., add = FALSE) {
  if(attr(.data, "active") != "nodes")
    stop("`group_by()` for cor_network only works on nodes.", call. = FALSE)
  gnodes <- with(.data$nodes, split(.data$nodes, ...))
  gnode.name <- lapply(gnodes, function(.x) {
    .data$nodes$name[.x$name]
  })
  gedges <- lapply(gnode.name, function(nm) {
    with(.data$edges[setdiff(names(.data$edges), c("r", "p.value"))],
         subset(.data$edges, all(c(.col.names, .row.names) %in% nm)))
  })
  gnet <- lapply(1:length(gnodes), function(.idx) {
    structure(.Data = list(nodes = gnodes[[.idx]], edges = gedges[[.idx]]),
              active = "nodes",
              class = class(.data))
  })
  structure(.Data = .data,
            grouped_cn = gnet,
            class = c("grouped_cor_network", class(.data)))
}

#' @importFrom dplyr ungroup
#' @export
ungroup.grouped_cor_network <- function(.data, ...)
{
  gnet <- attr(.data, "grouped_cn")
  nodes <- .data$nodes %>%
    left_join(dplyr::bind_rows(purrr::map(gnet, `[[`, 1)),
              by = c(name = "name"))
  edges <- .data$edges %>%
    left_join(dplyr::bind_rows(purrr::map(gnet, `[[`, 2)),
              by = c(.col.names = ".col.names", .row.names = ".row.names"))
  structure(.Data = list(nodes = nodes,
                         edges = edges),
            active = "nodes",
            class = "cor_network")
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

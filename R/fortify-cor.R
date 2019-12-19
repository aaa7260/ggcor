
#' @export
fortify_cor <- function(x,
                        y = NULL,
                        group = NULL,
                        type = "full",
                        show.diag = FALSE,
                        cor.test = FALSE,
                        cluster = FALSE,
                        cluster.method = "complete",
                        ...)
{
  type <- match.arg(type, c("full", "upper", "lower"))
  if(is_cor_tbl(x)) {
    return(
      switch (type,
      full = x,
      upper = get_upper_data(x, show.diag),
      lower = get_lower_data(x, show.diag)
    ))
  }
  clss <- c("correlation", "rcorr", "corr.test", "mantel_tbl")
  if(any(clss %in% class(x)) || (is.list(x) && !is.data.frame(x))) {
    return(as_cor_tbl(x, type = type, show.diag = show.diag, cluster = cluster,
                      cluster.method = cluster.method, ...))
  }
  y <- y %||% x
  if(!is.data.frame(x))
     x <- as.data.frame(x)
  if(!is.data.frame(y)) {
    y <- as.data.frame(y)
    if(nrow(x) != nrow(y))
      stop("'y' must have the same rows as 'x'.", call. = FALSE)
  }
  if(!is.null(group)) {
    if(length(group) != nrow(x))
      stop("'group' must have the same length as rows of 'x'.", call. = FALSE)
    x <- split(x, group, drop = FALSE)
    y <- split(y, group, drop = FALSE)
    df <- suppressMessages(
      pmap_dfr2(list(x, y, as.list(names(x))),
                function(.x, .y, .group) {
                  correlate(.x, .y, cor.test, ...) %>%
                    as_cor_tbl(type = type, show.diag = show.diag, cluster = cluster,
                               cluster.method = cluster.method) %>%
                    mutate2(group = .group)
                  })
      )
  } else {
    corr <- correlate(x, y, cor.test, ...)
    df <- as_cor_tbl(corr, type = type, show.diag = show.diag, cluster = cluster,
               cluster.method = cluster.method)
  }
  attr(df, "grouped") <- if(is.null(group)) FALSE else TRUE
  df
}

#' @noRd
pmap_dfr2 <- function(.l, .f, ..., keep.attrs = c("first", "last")) {
  keep.attrs <- match.arg(keep.attrs)
  ll <- purrr::pmap(.l, .f, ...)
  if(keep.attrs == "first") {
    attrs_all <- attributes(ll[[1]])
    attrs <- attrs_all[setdiff(names(attrs_all), c("names", "class", "row.names"))]
  } else {
    attrs_all <- attributes(ll[[length(ll)]])
    attrs <- attrs_all[setdiff(names(attrs_all), c("names", "class", "row.names"))]
  }
  df <- dplyr::bind_rows(ll)
  if(length(attrs) > 0) {
    purrr::walk(names(attrs), function(nm) {
      attr(df, nm) <<- attrs[[nm]]
    })
  }
  df
}

#' @noRd
mutate2 <- function(.data, ...) {
  attrs_all <- attributes(.data)
  attrs <- attrs_all[setdiff(names(attrs_all), c("names", "class", "row.names"))]
  df <- dplyr::mutate(.data, ...)
  if(length(attrs) > 0) {
    purrr::walk(names(attrs), function(nm) {
      attr(df, nm) <<- attrs[[nm]]
    })
  }
  df
}

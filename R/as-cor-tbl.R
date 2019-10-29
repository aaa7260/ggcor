#' @export
as_cor_tbl <- function(x, ...) {
  UseMethod("as_cor_tbl")
}
#' @export
as_cor_tbl.matrix <- function(x,
                              type = c("full", "upper", "lower"),
                              show.diag = TRUE,
                              p = NULL,
                              low = NULL,
                              upp = NULL,
                              cluster.type = c("none", "all", "row", "col"),
                              keep.name = FALSE,
                              ...) {
  x <- make_matrix_name(x)
  type <- match.arg(type)
  cluster.type <- match.arg(cluster.type)
  if(!is.null(p)) {
    if(!is.matrix(p))
      p <- as.matrix(p)
    if(any(dim(x) != dim(p)))
      stop("`p` must have the same dimension as `x`.", call. = FALSE)
  }
  if(!is.null(low)) {
    if(!is.matrix(low))
      low <- as.matrix(low)
    if(any(dim(x) != dim(low)))
      stop("`low` must have the same dimension as `x`.", call. = FALSE)
  }
  if(!is.null(upp)) {
    if(!is.matrix(upp))
      upp <- as.matrix(upp)
    if(any(dim(x) != dim(upp)))
      stop("`upp` must have the same dimension as `x`.", call. = FALSE)
  }
  if((length(rownames(x)) != length(colnames(x))) ||
     any(sort(rownames(x)) != sort(colnames(x)))) {
    show.diag <- "none"
    if(type != "full") {
      warning("'type=", type, "' just supports for symmetric correlation matrix.", call. = FALSE)
      type <- "full"
    }
  }
  if((type != "full") && (!cluster.type %in% c("none", "all"))) {
    warning("'cluster.type=", cluster.type, "' just spports for symmetric correlation matrix.", call. = FALSE)
    cluster.type <- "none"
  }
  ord <- matrix_order(x, cluster.type = cluster.type, ...)
  row_ord <- ord$row_ord
  col_ord <- ord$col_ord
  x <- x[row_ord, col_ord]
  xname <- colnames(x)
  yname <- rev(rownames(x))
  df <- matrix_to_df(x, "r")
  if(!is.null(p)) {
    p <- p[row_ord, col_ord]
    df$p <- as.vector(p)
  }
  if(!is.null(low)) {
    low <- low[row_ord, col_ord]
    df$low <- as.vector(low)
  }
  if(!is.null(upp)) {
    upp <- upp[row_ord, col_ord]
    df$upp <- as.vector(upp)
  }
  if(!keep.name) {
    df <- purrr::map_df(df, function(x) {
      if(is.factor(x)) {
        as.integer(x)
      } else x
    })
  }
  if(type == "full")
    show.diag <- TRUE
  if(keep.name) {
    class <- c("cor_tbl", "cor_tbl_name", class(df))
  } else {
    class <- c("cor_tbl", class(df))
  }
  out <- structure(
    .Data = df,
    xname = xname,
    yname = yname,
    type = type,
    show.diag = show.diag,
    class = class
  )
  switch (type,
          full = out,
          upper = get_upper_data(out, show.diag = show.diag),
          lower = get_lower_data(out, show.diag = show.diag)
  )
}
#' @export
as_cor_tbl.data.frame <- function(x,
                                  p = NULL,
                                  low = NULL,
                                  upp = NULL,
                                  cluster.type = c("none", "all", "row", "col"),
                                  keep.name = FALSE,
                                  ...) {
  x <- as.matrix(x)
  as_cor_tbl.matrix(x, p = p, low = low, upp = upp,
                    cluster.type = cluster.type, keep.name = keep.name, ...)
}

#' @export
as_cor_tbl.mantel_tbl <- function(x, byrow = TRUE, keep.name = FALSE, ...) {
  env.name <- unique(x$env)
  spec.name <- unique(x$spec)
  r <- x$r
  p <- x$p
  if(byrow) {
    xname <- env.name
    yname <- spec.name
    xx <- factor(x$env, levels = xname)
    yy <- factor(x$spec, levels = rev(yname))
  } else {
    xname <- spec.name
    yname <- env.name
    xx <- factor(x$spec, levels = xname)
    yy <- factor(x$env, levels = rev(yname))
  }
  df <- tibble::tibble(x = xx, y = yy, r = r, p = p)
  if(!keep.name) {
    df <- purrr::map_df(df, function(x) {
      if(is.factor(x)) {
        as.integer(x)
      } else x
    })
  }
  if(keep.name) {
    class <- c("cor_tbl", "cor_tbl_name", class(df))
  } else {
    class <- c("cor_tbl", class(df))
  }
  out <- structure(
    .Data = df,
    xname = xname,
    yname = yname,
    type = "full",
    show.diag = "none",
    class = class
  )
  out
}

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
  df <- purrr::map_df(df, as.numeric)
  if(type == "full")
    show.diag <- TRUE
  out <- structure(
    .Data = df,
    xname = xname,
    yname = yname,
    type = type,
    show.diag = show.diag,
    class = c("cor_tbl", class(df))
  )
  cor_tbl_check(out)
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
                                  ...) {
  x <- as.matrix(x)
  as_cor_tbl.matrix(x, p = p, low = low, upp = upp,
                    cluster.type = cluster.type, ...)
}

#' @export
as_cor_tbl.mantel_tbl <- function(x, byrow = TRUE) {
  env.name <- unique(x$env)
  spec.name <- unique(x$spec)
  r <- x$r
  p <- x$p
  if(byrow) {
    xname <- env.name
    yname <- spec.name
    xx <- as.integer(factor(x$env, levels = xname))
    yy <- as.integer(factor(x$spec, levels = rev(yname)))
  } else {
    xname <- spec.name
    yname <- env.name
    xx <- as.integer(factor(x$spec, levels = xname))
    yy <- as.integer(factor(x$env, levels = rev(yname)))
  }
  df <- tibble::tibble(x = xx, y = yy, r = r, p = p)
  out <- structure(
    .Data = df,
    xname = xname,
    yname = yname,
    type = "full",
    show.diag = "none",
    class = c("cor_tbl", class(df))
  )
  cor_tbl_check(out)
  out
}

#' @export
print.cor_tbl <- function(x, nmax = 5, width = 60) {
  xx <- purrr::map_df(x, function(x) {if(is.numeric(x))
    format(x, digits = 1, nsmall = 2)})
  n <- nrow(x)
  nn <- min(nmax, nrow(x))
  lapply(0:(nn + 1), function(i) {
    if(i == 0) cat_line(names(x), collapse = "\t", width = width)
    if(i == nn + 1 && nn < n) cat("# ... with", n - nn, " more rows\n")
    if(i > 0 && i < nn + 1) cat_line(xx[i, ], collapse = "\t")
  })
  cat("Extra attributes:\n")
  cat_line(cor_tbl_xname(x), name = "xname:", width = width)
  cat_line(cor_tbl_yname(x), name = "yname:", width = width)
  cat_line(cor_tbl_showdiag(x), name = "show.diag:", width = width)
}


#' @noRd
plot.cor_tbl <- function(x, ...) {
  ggcor(x, ...) + geom_raster()
}

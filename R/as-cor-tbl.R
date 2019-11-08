#' Coerce to a cor_tbl object.
#' @description Functions to coerce a object to cor_tbl if possible.
#' @param corr Any \code{R} object.
#' @param type A string, "full" (default), "upper" or "lower", display full
#'     matrix, lower triangular or upper triangular matrix.
#' @param show.diag A logical value indicating whether keep the diagonal.
#' @param p Matrix of p value.
#' @param low Matrix of the lower bound of confidence interval.
#' @param upp Matrix of the upper bound of confidence interval.
#' @param cluster.type A string, the ordering type of the correlation matrix.
#'     \itemize{
#'   \item{\code{"none"} for original order (default).}
#'   \item{\code{"all"} for reordering rows and columns at the same time.}
#'   \item{\code{"row"} for reordering rows, just supports the symmetry correlation matrix.}
#'   \item{\code{"col"} for reordering columns, just supports the symmetry correlation matrix.}
#' }
#' @param keep.name A logical value indicating whether keep the x/y column name.
#' @param ... Extra params, see Details.
#' @details The method of coerce a \code{matrix} object to a cor_tbl object is the
#'     fundamental function. In the earth, other methods is call the \code{as_cor_tbl.matrix}
#'     coerce to cor_tbl except \code{as_cor_tbl.mantel_tbl}.
#'     For \code{as_cor_tbl.matrix}, \code{...} params pass to \code{\link[ggcor]{matrix_order}},
#'     and for \code{as_cor_tbl.data.frame}, \code{...} params pass to \code{as_cor_tbl.matrix}.
#' @return A cor_tbl object.
#' @rdname as-cor-tbl
#' @export
#' @examples
#' corr <- cor(mtcars)
#' as_cor_tbl(corr)
#' @author Houyun Huang
#' @author Lei Zhou
#' @author Jian Chen
#' @author Taiyun Wei
as_cor_tbl <- function(corr, ...) {
  UseMethod("as_cor_tbl")
}
#' @rdname  as-cor-tbl
#' @export
#' @method as_cor_tbl matrix
as_cor_tbl.matrix <- function(corr,
                              type = c("full", "upper", "lower"),
                              show.diag = TRUE,
                              p = NULL,
                              low = NULL,
                              upp = NULL,
                              cluster.type = c("none", "all", "row", "col"),
                              keep.name = FALSE,
                              ...) {
  corr <- make_matrix_name(corr)
  type <- match.arg(type)
  cluster.type <- match.arg(cluster.type)
  if(!is.null(p)) {
    if(!is.matrix(p))
      p <- as.matrix(p)
    if(any(dim(corr) != dim(p)))
      stop("`p` must have the same dimension as `corr`.", call. = FALSE)
  }
  if(!is.null(low)) {
    if(!is.matrix(low))
      low <- as.matrix(low)
    if(any(dim(corr) != dim(low)))
      stop("`low` must have the same dimension as `corr`.", call. = FALSE)
  }
  if(!is.null(upp)) {
    if(!is.matrix(upp))
      upp <- as.matrix(upp)
    if(any(dim(corr) != dim(upp)))
      stop("`upp` must have the same dimension as `corr`.", call. = FALSE)
  }
  if((length(rownames(corr)) != length(colnames(corr))) ||
     any(sort(rownames(corr)) != sort(colnames(corr)))) {
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
  ord <- matrix_order(corr, cluster.type = cluster.type, ...)
  row_ord <- ord$row_ord
  col_ord <- ord$col_ord
  corr <- corr[row_ord, col_ord]
  xname <- colnames(corr)
  yname <- rev(rownames(corr))
  df <- matrix_to_df(corr, "r")
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
    class <- c("cor_tbl_fct", class(df))
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
#' @rdname  as-cor-tbl
#' @export
#' @method as_cor_tbl data.frame
as_cor_tbl.data.frame <- function(corr,
                                  p = NULL,
                                  low = NULL,
                                  upp = NULL,
                                  cluster.type = c("none", "all", "row", "col"),
                                  keep.name = FALSE,
                                  ...) {
  corr <- as.matrix(corr)
  as_cor_tbl.matrix(corr, p = p, low = low, upp = upp,
                    cluster.type = cluster.type, keep.name = keep.name, ...)
}

#' @rdname  as-cor-tbl
#' @export
#' @method as_cor_tbl rcorr
as_cor_tbl.rcorr <- function(corr, fill.diag.p = 0, ...)
{
  if(!is.null(fill.diag.p)) {
    p <- corr$P
    diag(p) <- 0
  } else {
    p <- corr$P
  }
  df <- as_cor_tbl.matrix(corr$r, p = p, keep.name = FALSE, ...)
  df
}


#' @rdname  as-cor-tbl
#' @export
#' @method as_cor_tbl cor_tbl_fct
as_cor_tbl.cor_tbl_fct <- function(corr, ...) {
  corr$x <- as.integer(corr$x)
  corr$y <- as.integer(corr$y)
  class(corr) <- c("cor_tbl", setdiff(class(corr), "cor_tbl_fct"))
}

#' @rdname  as-cor-tbl
#' @export
#' @method as_cor_tbl mantel_tbl
as_cor_tbl.mantel_tbl <- function(corr, byrow = TRUE, ...) {
  env.name <- unique(corr$env)
  spec.name <- unique(corr$spec)
  r <- corr$r
  p <- corr$p
  if(byrow) {
    xname <- env.name
    yname <- spec.name
    xx <- factor(corr$env, levels = xname)
    yy <- factor(corr$spec, levels = rev(yname))
  } else {
    xname <- spec.name
    yname <- env.name
    xx <- factor(corr$spec, levels = xname)
    yy <- factor(corr$env, levels = rev(yname))
  }
  df <- tibble::tibble(x = as.integer(xx), y = as.integer(yy), r = r, p = p)
  out <- structure(
    .Data = df,
    xname = xname,
    yname = yname,
    type = "full",
    show.diag = "none",
    class = c("cor_tbl", setdiff(class(df), "mantel_tbl"))
  )
  out
}

#' @rdname as-cor-tbl
#' @export
#' @method as_cor_tbl default
as_cor_tbl.default <- function(corr, ...) {
  stop(class(corr), " hasn't been realized yet.", call. = FALSE)
}

#' @export
as_cor_tbl_fct <- function(corr, ...) {
  UseMethod("as_cor_tbl_fct")
}

#' @export
as_cor_tbl_fct.cor_tbl <- function(corr, ...) {
  xname <- cor_tbl_xname(corr)
  yname <- cor_tbl_yname(corr)
  corr$x <- factor(xname[corr$x], levels = xname)
  corr$y <- factor(yname[corr$y], levels = yname)
  structure(corr,
            class = c("cor_tbl_fct", setdiff(class(corr), "cor_tbl")))
}
#' @export
as_cor_tbl_fct.rcorr <- function(corr, fill.diag.p = 0, ...)
{
  if(!is.null(fill.diag.p)) {
    p <- corr$P
    diag(p) <- 0
  } else {
    p <- corr$P
  }
  df <- as_cor_tbl.matrix(corr$r, p = p, keep.name = TRUE, ...)
  df
}

#' @export
as_cor_tbl_fct.mantel_tbl <- function(corr, byrow = TRUE, ...) {
  env.name <- unique(corr$env)
  spec.name <- unique(corr$spec)
  r <- corr$r
  p <- corr$p
  if(byrow) {
    xname <- env.name
    yname <- spec.name
    xx <- factor(corr$env, levels = xname)
    yy <- factor(corr$spec, levels = rev(yname))
  } else {
    xname <- spec.name
    yname <- env.name
    xx <- factor(corr$spec, levels = xname)
    yy <- factor(corr$env, levels = rev(yname))
  }
  df <- tibble::tibble(x = xx, y = yy, r = r, p = p)
  out <- structure(
    .Data = df,
    xname = xname,
    yname = yname,
    type = "full",
    show.diag = "none",
    class = c("cor_tbl_fct", setdiff(class(df), "mantel_tbl"))
  )
  out
}
#' @export
as_cor_tbl_fct.default <- function(corr, ...) {
  stop(class(corr), " hasn't been realized yet.", call. = FALSE)
}

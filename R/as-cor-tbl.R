#' Coerce to a cor_tbl object.
#' @description Functions to coerce a object to cor_tbl if possible.
#' @param corr any \code{R} object.
#' @param type a string, "full" (default), "upper" or "lower", display full,
#'     lower triangular or upper triangular matrix.
#' @param show.diag a logical value indicating whether keep the diagonal.
#' @param p.value a matrix of p value.
#' @param lower.ci,upper.ci matrix of confidence interval.
#' @param rownames,colnames row/column names of correlation matrix.
#' @param cluster a logical value indicating whether reorder the correlation matrix
#'     by clustering, default is FALSE.
#' @param byrow a logical value indicating whether arrange the 'spec' columns on y axis.
#' @param keys a named character vector, should contain "r" and "p.value".
#' @param check a logical value indicating whether check the correlation coefficient and
#'     p value.
#' @param ... extra params passing to \code{\link[ggcor]{matrix_order}}.
#' @details \code{cluster = TRUE} just supports for symmetric correlation matrix.
#' @return a cor_tbl object.
#' @rdname as_cor_tbl
#' @export
#' @examples
#' corr <- cor(mtcars)
#' as_cor_tbl(corr)
#' ll <- correlate(mtcars)
#' as_cor_tbl(ll, type = "upper")
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
as_cor_tbl <- function(corr, ...) {
  UseMethod("as_cor_tbl")
}
#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl matrix
as_cor_tbl.matrix <- function(corr,
                              type = "full",
                              show.diag = TRUE,
                              p.value = NULL,
                              lower.ci = NULL,
                              upper.ci = NULL,
                              row.names = NULL,
                              col.names = NULL,
                              cluster = FALSE,
                              check = TRUE,
                              ...) {
  type <- match.arg(type, c("full", "upper", "lower"))
  if(!is.null(row.names))
    rownames(corr) <- row.names
  if(!is.null(col.names))
    rownames(corr) <- col.names
  corr <- make_matrix_name(corr)
  xname <- colnames(corr)
  yname <- rev(rownames(corr))
  if(!is.null(p.value)) {
    if(!is.matrix(p.value))
      p.value <- as.matrix(p.value)
  }
  if(check) {
    check_cor_matrix(corr, p.value)
  } else {
    check_dimension(corr, p.value)
  }
  if(!is.null(lower.ci)) {
    if(!is.matrix(lower.ci))
      lower.ci <- as.matrix(lower.ci)
    check_dimension(corr, lower.ci)
  }
  if(!is.null(upper.ci)) {
    if(!is.matrix(upper.ci))
      upper.ci <- as.matrix(upper.ci)
    check_dimension(corr, upper.ci)
  }
  if(!isSymmetric(corr) || any(xname != rev(yname))) {
    if(type != "full") {
      warning("'type=", type, "' just supports for symmetric correlation matrix.", call. = FALSE)
      type <- "full"
      if(type == "full") show.diag <- TRUE
    }
    if(isTRUE(cluster)) {
      warning("'cluster' just spports for symmetric correlation matrix.", call. = FALSE)
      cluster <- FALSE
    }
  }
  if(isTRUE(cluster)) {
    ord <- matrix_order(corr, ...)
    corr <- corr[ord, ord]
    p.value <- if(is.null(p.value)) p.value else p.value[ord, ord]
    lower.ci <- if(is.null(lower.ci)) lower.ci else lower.ci[ord, ord]
    upper.ci <- if(is.null(upper.ci)) upper.ci else upper.ci[ord, ord]
  }
  df <- make_cor_tbl(corr, p.value, lower.ci, upper.ci)
  cor_tbl <- structure(
    .Data = df,
    xname = xname,
    yname = yname,
    type = type,
    show.diag = show.diag,
    grouped = FALSE,
    class = c("cor_tbl", class(df))
  )
  switch (type,
          full = cor_tbl,
          upper = get_upper_data(cor_tbl, show.diag = show.diag),
          lower = get_lower_data(cor_tbl, show.diag = show.diag)
  )
}
#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl data.frame
as_cor_tbl.data.frame <- function(corr,
                                  p.value = NULL,
                                  lower.ci = NULL,
                                  upper.ci = NULL,
                                  cluster = FALSE,
                                  ...) {
  corr <- as.matrix(corr)
  as_cor_tbl(corr, p.value = p.value, lower.ci = lower.ci,
                    upper.ci = upper.ci, cluster = cluster, ...)
}

#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl correlation
as_cor_tbl.correlation <- function(corr, check = FALSE, ...) {
  as_cor_tbl(corr$r, p.value = corr$p.value, lower.ci = corr$lower.ci,
             upper.ci = corr$upper.ci, check = FALSE, ...)
}

#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl rcorr
as_cor_tbl.rcorr <- function(corr, check = FALSE, ...)
{
  p.value <- corr$P
  diag(p.value) <- 0
  as_cor_tbl(corr$r, p.value = p.value, check = FALSE, ...)
}

#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl corr.test
as_cor_tbl.corr.test <- function(corr, check = FALSE, ...)
{
  as_cor_tbl(corr$r, p.value = corr$p, check = FALSE, ...)
}

#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl mantel_tbl
as_cor_tbl.mantel_tbl <- function(corr, byrow = TRUE, ...) {
  env_nm <- unique(corr$env)
  spec_nm <- unique(corr$spec)
  if(byrow) {
    xname <- env_nm
    yname <- spec_nm
    idx <- corr$env
    idy <- corr$spec
    x <- as.integer(factor(corr$env, levels = xname))
    y <- as.integer(factor(corr$spec, levels = rev(yname)))
  } else {
    xname <- spec_nm
    yname <- env_nm
    idx <- corr$spec
    idy <- corr$env
    x <- as.integer(factor(corr$spec, levels = xname))
    y <- as.integer(factor(corr$env, levels = rev(yname)))
  }
  df <- tibble::tibble(idx = idx, idy = idy, r = corr$r,
                       p.value = corr$p.value, x = x, y = y)
  if(attr(corr, "grouped"))
    df$group <- corr$group
  structure(
    .Data = df,
    xname = xname,
    yname = yname,
    type = "full",
    show.diag = TRUE,
    grouped = attr(corr, "grouped"),
    class = c("cor_tbl", setdiff(class(df), "mantel_tbl"))
  )
}

#' @rdname as_cor_tbl
#' @export
#' @method as_cor_tbl list
as_cor_tbl.list <- function(corr, keys = NULL, check = TRUE, ...)
{
  if(is.null(keys)) {
    r <- corr$r %||% corr$cor %||% corr$corr
    p <- corr$p %||% corr$pvalue %||% corr$p.value
  } else {
    nm <- names(keys)
    if(is.null(nm))
      stop("'keys' must be a named character vector.", call. = FALSE)
    if(!"r" %in% nm )
      stop("'keys' must contained 'r' column.", call. = FALSE)
    r <- corr[[keys["r"]]]
    p <- if(is.null(keys["p.value"])) NULL else corr[[keys["p.value"]]]
  }
  if(is.null(r)) {
    stop("Not find correlation matrix.", call. = FALSE)
  }
  as_cor_tbl(r, p.value = p, check = check, ...)
}
#' @rdname as_cor_tbl
#' @export
#' @method as_cor_tbl default
as_cor_tbl.default <- function(corr, ...) {
  stop(class(corr), " hasn't been realized yet.", call. = FALSE)
}

#' @noRd
check_dimension <- function(x, y) {
  x_nm <- as.character(match.call()[["x"]])
  y_nm <- as.character(match.call()[["y"]])
  if(any(dim(x) != dim(y))) {
    msg <- paste0(" Dimension error: ", y_nm, " must have same dimension as ", x_nm)
    stop(msg, call. = FALSE)
  }
}

#' @noRd
check_cor_matrix <- function(corr,
                             p.value = NULL)
{
  if(!is.numeric(corr))
    stop("'corr' needs a numeric matrix.", call. = FALSE)
  if(!is.null(p.value))
    check_dimension(corr, p.value)
  corr <- corr[is.finite(corr)]
  if(!all(corr >= -1 & corr <= 1))
    stop("'corr' not in range -1 to 1.", call. = FALSE)
  if(!is.null(p.value)) {
    if(!is.numeric(p.value))
      stop("'p.value' needs a numeric matrix.", call. = FALSE)
    p.value <- p.value[is.finite(p.value)]
    if(!all(p.value >= 0 & p.value <= 1))
      stop("'p.value' not in range 0 to 1.", call. = FALSE)
  }
}

#' @noRd
make_cor_tbl <- function(corr,
                         p.value = NULL,
                         lower.ci = NULL,
                         upper.ci = NULL)
{
  row_nm <- rownames(corr)
  col_nm <- colnames(corr)
  n <- nrow(corr)
  m <- ncol(corr)
  tibble::tibble(idx = rep(col_nm, each = n),
                 idy = rep(row_nm, m)) %>%
    dplyr::mutate(
      r = as.vector(corr),
      p.value = as.vector(p.value),
      lower.ci = as.vector(lower.ci),
      upper.ci = as.vector(upper.ci),
      x = rep(1:m, each= n),
      y = rep(n:1, m))
}

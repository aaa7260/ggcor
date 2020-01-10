#' Coerce to a cor_tbl object
#' @description Functions to coerce a object to cor_tbl if possible.
#' @param x any \code{R} object.
#' @param type a string, "full" (default), "upper" or "lower", display full,
#'     lower triangular or upper triangular matrix.
#' @param show.diag a logical value indicating whether keep the diagonal.
#' @param row.names,col.names row/column names of correlation matrix.
#' @param cluster a logical value indicating whether reorder the correlation matrix
#'     by clustering, default is FALSE.
#' @param byrow a logical value indicating whether arrange the 'spec' columns on y axis.
#' @param ... extra params passing to \code{\link[ggcor]{matrix_order}}.
#' @details \code{cluster = TRUE} just supports for symmetric correlation matrix.
#' @return a cor_tbl object.
#' @importFrom utils modifyList
#' @rdname as_cor_tbl
#' @examples
#' corr <- cor(mtcars)
#' as_cor_tbl(corr)
#' ll <- correlate(mtcars)
#' as_cor_tbl(ll, type = "upper")
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
as_cor_tbl <- function(x, ...) {
  UseMethod("as_cor_tbl")
}
#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl list
as_cor_tbl.list <- function(x,
                            type = "full",
                            show.diag = TRUE,
                            row.names = NULL,
                            col.names = NULL,
                            cluster = FALSE,
                            ...)
{
  type <- match.arg(type, c("full", "upper", "lower"))
  ## exclude NULL
  x <- modifyList(list(), x, keep.null = FALSE)
  x <- set_list_name(x)
  if(length(x) == 0) {
    return(new_data_frame())
  }
  name <- names(x)
  if(is.null(name) || length(unique(name)) != length(x))
    stop("'x' must be a all named list.", call. = FALSE)
  first <- x[[1]]
  x <- lapply(x, function(.x) {
    if(!is.matrix(.x)) as.matrix(.x) else .x
  })
  if(length(x) > 1) {
    lapply(names(x), function(name) {
      check_dimension(first, x[[name]])
    })
  }
  row.names <- row.names %||% rownames(first) %||% paste0("row", 1:nrow(first))
  col.names <- col.names %||% colnames(first) %||% paste0("col", 1:ncol(first))
  if(length(row.names) != nrow(first))
    stop("'row.names' must have same length as rows of matrix.", call. = FALSE)
  if(length(col.names) != ncol(first))
    stop("'col.names' must have same length as columns of matrix.", call. = FALSE)

  ## handle cluster
  if(!isSymmetric(first) || any(colnames(first) != rownames(first))) {
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
    ord <- matrix_order(first, ...)
    x <- lapply(x, function(.x) {
      .x[ord, ord]
    })
    row.names <- row.names[ord]
    col.names <- col.names[ord]
  }
  id <- list(.row.names = rep(row.names, ncol(first)),
             .col.names = rep(col.names, each = nrow(first)),
             .row.id = rep(nrow(first):1, ncol(first)),
             .col.id = rep(1:ncol(first), each = nrow(first))
  )
  data <- modifyList(id, setNames(lapply(x, as.vector), name))
  new.order <- intersect(c(".row.names", ".col.names", name, ".row.id", ".col.id"),
                         names(data))
  data <- structure(.Data = new_data_frame(data[new.order]),
                    .row.names = row.names,
                    .col.names = col.names,
                    type = type,
                    show.diag = show.diag,
                    grouped = FALSE,
                    class = c("cor_tbl", "tbl_df", "tbl", "data.frame"))
  switch (type,
          full = data,
          upper = get_upper_data(data, show.diag = show.diag),
          lower = get_lower_data(data, show.diag = show.diag)
  )
}
#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl matrix
as_cor_tbl.matrix <- function(x, ...) {
  as_cor_tbl(list(r = x), ...)
}
#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl data.frame
as_cor_tbl.data.frame <- function(x, ...) {
  as_cor_tbl(list(r = data.matrix(x)), ...)
}

#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl correlation
as_cor_tbl.correlation <- function(x, ...) {
  as_cor_tbl.list(x, ...)
}

#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl rcorr
as_cor_tbl.rcorr <- function(x, ...)
{
  p.value <- x$P
  diag(p.value) <- 0
  as_cor_tbl(list(r = x$r, p.value = p.value), ...)
}

#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl corr.test
as_cor_tbl.corr.test <- function(x, ...)
{
  as_cor_tbl(list(x$r, p.value = x$p), ...)
}
#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl mantel_tbl
as_cor_tbl.mantel_tbl <- function(x, byrow = TRUE, ...) {
  env_nm <- unique(x$env)
  spec_nm <- unique(x$spec)
  if(byrow) {
    col.names <- env_nm
    row.names <- spec_nm
    .col.names <- x$env
    .row.names <- x$spec
    .col.id <- as.integer(factor(x$env, levels = col.names))
    .row.id <- as.integer(factor(x$spec, levels = rev(row.names)))
  } else {
    col.names <- spec_nm
    row.names <- env_nm
    .col.names <- x$spec
    .row.names <- x$env
    .col.id <- as.integer(factor(x$spec, levels = col.names))
    .row.id <- as.integer(factor(x$env, levels = rev(row.names)))
  }
  df <- tibble::tibble(.col.names = .col.names, .row.names = .row.names,
                       r = x$r, p.value = x$p.value, .row.id = .row.id,
                       .col.id = .col.id)
  structure(
    .Data = df,
    .row.names = .row.names,
    .col.names = .col.names,
    type = "full",
    show.diag = TRUE,
    grouped = attr(x, "grouped"),
    class = c("cor_tbl", setdiff(class(df), "mantel_tbl"))
  )
}
#' @rdname as_cor_tbl
#' @export
#' @method as_cor_tbl default
as_cor_tbl.default <- function(x, ...) {
  stop(class(x)[1], " hasn't been realized yet.", call. = FALSE)
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
set_list_name <- function(x) {
  if(is.matrix(x) || is.data.frame(x))
    x <- list(x)
  if(!is.list(x)) {
    stop("'x' needs a list object.", call. = FALSE)
  }
  name <- names(x)
  if(is.null(name)) {
    name <- paste0("m", 1:length(x))
  }
  name <- make.unique(name, "")
  names(x) <- name
  x
}

#' Create a cor_tbl object
#' @title Create a cor_tbl object
#' @description Functions to create cor_tbl object from correlation matrix.
#' @param corr correlation matrix.
#' @param p.value significance value matrix of correaltion.
#' @param extra.mat any other matrix-like data with same dimmsion as \code{x}.
#' @param type a string, "full" (default), "upper" or "lower", display full,
#'     lower triangular or upper triangular matrix.
#' @param show.diag a logical value indicating whether keep the diagonal.
#' @param row.names,col.names row/column names of correlation matrix.
#' @param row.order,col.order row/column order of correlation matrix.
#' @param cluster a logical value indicating whether reorder the correlation matrix
#'     by clustering, default is FALSE.
#' @param ... extra params passing to \code{\link[ggcor]{cor_tbl}}.
#' @details \code{cluster = TRUE} just supports for symmetric correlation matrix.
#' @return a cor_tbl object.
#' @importFrom utils modifyList
#' @rdname cor_tbl
#' @examples
#' cor_tbl(cor(mtcars))
#' corr <- correlate(mtcars, cor.test = TRUE)
#'
#' ## with p value
#' cor_tbl(corr$r, corr$p.value)
#'
#' ## reorder correlation matrix
#' cor_tbl(corr$r, corr$p.value, cluster = TRUE)
#'
#' ## exclude upper or lower
#' ### exclude lower
#' cor_tbl(corr$r, corr$p.value, type = "upper")
#' ### exclude upper
#' cor_tbl(corr$r, corr$p.value, type = "lower", show.diag = FALSE)
#'
#' ## add extra matrix data
#' m <- matrix(rnorm(11*11), nrow = 11)
#' cor_tbl(corr$r, corr$p.value, extra.mat = list(m = m))
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
cor_tbl <- function(corr,
                    p.value = NULL,
                    extra.mat = list(),
                    type = "full",
                    show.diag = TRUE,
                    row.names = NULL,
                    col.names = NULL,
                    row.order = NULL,
                    col.order = NULL,
                    cluster = FALSE,
                    ...)
{
  type <- match.arg(type, c("full", "upper", "lower"))
  check_extra_mat_name(extra.mat)
  ## exclude NULL
  missing.corr <- missing(corr)
  if(missing.corr && is.null(p.value) && length(extra.mat) == 0) {
    stop("No matrix data input.", call. = FALSE)
  }

  first <- if(missing.corr) {
    if(length(extra.mat) == 0) p.value else extra.mat[[1]]
  } else corr
  if(!is.matrix(first))
    first <- as.matrix(first)

  if(missing.corr) {
    corr <- if(is.null(p.value)) list() else list(p.value = p.value)
  } else {
    corr <- if(is.null(p.value)) list(r = corr) else list(r = corr, p.value = p.value)
  }

  corr <- modifyList(corr, extra.mat)

  corr <- lapply(corr, function(.x) {
    if(!is.matrix(.x)) as.matrix(.x) else .x
  })
  name <- names(corr)

  if(length(corr) > 1) {
    lapply(names(corr), function(name) {
      check_dimension(first, corr[[name]])
    })
  }
  row.names <- row.names %||% rownames(first) %||% paste0("row", 1:nrow(first))
  col.names <- col.names %||% colnames(first) %||% paste0("col", 1:ncol(first))
  if(length(row.names) != nrow(first))
    stop("'row.names' must have same length as rows of matrix.", call. = FALSE)
  if(length(col.names) != ncol(first))
    stop("'col.names' must have same length as columns of matrix.", call. = FALSE)

  ## check type
  if(missing.corr) {
    if(nrow(first) != ncol(first)) {
      if(type != "full") {
        warning("'type=", type,
                "' just supports for square matrix.", call. = FALSE)
        type <- "full"
        if(type == "full") show.diag <- TRUE
      }
    }
  } else {
    if(!isSymmetric(first) || any(colnames(first) != rownames(first))) {
      if(type != "full") {
        warning("'type=", type, "' just supports for symmetric matrix.", call. = FALSE)
        type <- "full"
        if(type == "full") show.diag <- TRUE
      }
    }
  }

  ## check cluster and row/col.order
  not.null.order <- any(!is.null(row.order), !is.null(col.order))
  row.hc <- col.hc <- NULL
  if(not.null.order) {
    if(isTRUE(cluster)) {
      warning("'row/col.order' has been specified, cluster will not be used.", call. = FALSE)
      cluster <- FALSE
    }
    row.ord <- 1:length(row.names)
    col.ord <- 1:length(col.names)
    if(!is.null(row.order)) {
      row.ord <- get_order(row.order)
      if(is.character(row.order)) {
        row.ord <- row.ord[row.names]
      }
      if(inherits(row.order, "hclust") || inherits(row.order, "dendrogram")) {
        row.hc <- row.order
      }
    }

    if(!is.null(col.order)) {
      col.ord <- get_order(col.order)
      if(is.character(col.order)) {
        col.ord <- col.ord[col.names]
      }
      if(inherits(col.order, "hclust") || inherits(col.order, "dendrogram")) {
        col.hc <- col.order
      }
    }
  }

  if(isTRUE(cluster)) {
    hc <- matrix_order(first, is.cor = !missing.corr, ...)
    row.hc <- hc$row.hc
    col.hc <- hc$col.hc
    row.ord <- row.hc$order
    col.ord <- col.hc$order
  }

  if(not.null.order || isTRUE(cluster)) {
    corr <- lapply(corr, function(.x) {
      .x[row.ord, col.ord]
    })
    row.names <- row.names[row.ord]
    col.names <- col.names[col.ord]
  }
  id <- list(
    .row.names = rep(row.names, ncol(first)),
    .col.names = rep(col.names, each = nrow(first)),
    .row.id = rep(nrow(first):1, ncol(first)),
    .col.id = rep(1:ncol(first), each = nrow(first))
  )
  data <- modifyList(id, setNames(lapply(corr, as.vector), name))
  new.order <- intersect(c(".row.names", ".col.names", name, ".row.id", ".col.id"),
                         names(data))
  cls <- if(missing.corr) {
    c("general_cor_tbl", "cor_tbl", "tbl_df", "tbl", "data.frame")
  } else {
    c("cor_tbl", "tbl_df", "tbl", "data.frame")
  }
  data <- structure(.Data = new_data_frame(data[new.order]),
                    .row.names = row.names,
                    .col.names = col.names,
                    type = type,
                    show.diag = show.diag,
                    hclust = list(row.hc = row.hc, col.hc = col.hc),
                    grouped = FALSE,
                    class = cls)
  switch (type,
          full = data,
          upper = get_upper_data(data, show.diag = show.diag),
          lower = get_lower_data(data, show.diag = show.diag)
  )
}

#' Create a general cor_tbl object
#' @title Create a general cor_tbl object
#' @param x a matrix-like object.
#' @param name variable name.
#' @param extra.mat extra matrix data.
#' @param ... parameters passing to \code{cor_tbl}.
#' @return a general_cor_tbl object.
#' @rdname gcor_tbl
#' @examples
#' gcor_tbl(mtcars)
#' @export
gcor_tbl <- function(x,
                     name = NULL,
                     extra.mat = list(),
                     ...)
{
  if("corr" %in% names(list(...))) {
    stop("please use `cor_tbl()` to tidy correlation matrix.", call. = FALSE)
  }
  nm <- name %||% deparse(substitute(x))
  extra.mat[[nm]] <- x
  cor_tbl(extra.mat = extra.mat, ...)
}

#' @noRd
check_extra_mat_name <- function(l)
{
  n <- length(l)
  if(n > 0) {
    name <- names(l)
    if(is.null(name) || length(unique(name)) != n) {
      stop(
        "Names of extra.mat check:", "\n",
        "The elements of 'extra.mat' must with unique name.",
        call. = FALSE)
    }
    if(any(name %in% c("r", "p.value"))) {
      stop(
        "Names of extra.mat check:", "\n",
        "'r' and 'p.value' are preserved.",
        call. = FALSE)
    }
  }
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


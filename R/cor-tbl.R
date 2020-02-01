#' Create a cor_tbl object
#' @description Functions to create cor_tbl object from correlation matrix.
#' @param corr correlation matrix.
#' @param p.value significance value matrix of correaltion.
#' @param extra.mat any other matrix-like data with same dimmsion as \code{x}.
#' @param type a string, "full" (default), "upper" or "lower", display full,
#'     lower triangular or upper triangular matrix.
#' @param show.diag a logical value indicating whether keep the diagonal.
#' @param row.names,col.names row/column names of correlation matrix.
#' @param cluster a logical value indicating whether reorder the correlation matrix
#'     by clustering, default is FALSE.
#' @param ... extra params passing to \code{\link[ggcor]{cor_tbl}}.
#' @details \code{cluster = TRUE} just supports for symmetric correlation matrix.
#' @return a cor_tbl object.
#' @importFrom utils modifyList
#' @rdname cor_tbl
#' @examples
#' corr <- cor(mtcars)
#' cor_tbl(corr)
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
cor_tbl <- function(corr,
                    p.value = NULL,
                    extra.mat = list(),
                    type = "full",
                    show.diag = TRUE,
                    row.names = NULL,
                    col.names = NULL,
                    cluster = FALSE,
                    ...)
{
  type <- match.arg(type, c("full", "upper", "lower"))
  check_extra_mat_name(extra.mat)
  ## exclude NULL
  corr <- if(is.null(p.value)) {
    list(r = corr)
    } else {
      list(r = corr, p.value = p.value)
    }

  corr <- modifyList(corr, extra.mat)

  corr <- lapply(corr, function(.x) {
    if(!is.matrix(.x)) as.matrix(.x) else .x
  })

  first <- corr[[1]]
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
    corr <- lapply(corr, function(.x) {
      .x[ord, ord]
    })
    row.names <- row.names[ord]
    col.names <- col.names[ord]
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

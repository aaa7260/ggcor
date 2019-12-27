#' @noRd
make_matrix_name <- function(mat)
{
  if(!is.matrix(mat))
    mat <- as.matrix(mat)
  n <- nrow(mat)
  m <- ncol(mat)
  col_name <- colnames(mat)
  row_name <- rownames(mat)
  # check row names
  if(is.null(row_name)) {
    rownames(mat) <- paste0("Y", seq_len(n))
  }
  if(!is.null(row_name) && all(row_name == "" | is.na(row_name))) {
    rownames(mat) <- paste0("Y", seq_len(n))
  }
  if(!is.null(row_name) && any(row_name == "" | is.na(row_name))) {
    row_idx <- row_name == "" | is.na(row_name)
    row_name[row_idx] <- paste0("Y", seq_len(sum(row_idx)))
    rownames(mat) <- make.unique(row_name)
  }
  # check column names
  if(is.null(col_name)) {
    colnames(mat) <- paste0("X", seq_len(m))
  }
  if(!is.null(col_name) && all(col_name == "" | is.na(col_name))) {
    colnames(mat) <- paste0("X", seq_len(m))
  }
  if(!is.null(col_name) && any(col_name == "" | is.na(col_name))) {
    col_idx <- col_name == "" | is.na(col_name)
    col_name[row_idx] <- paste0("X", seq_len(sum(col_idx)))
    colnames(mat) <- make.unique(col_name)
  }
  mat
}
#' @noRd
make_list_names <- function(x, pre = "X", sep = "")
{
  stopifnot(is.list(x))
  n <- length(x)
  name <- names(x)
  if(!is.null(name) && all(name != "" & !is.na(name)))
    return(x)
  if(is.null(x)) {
    names(x) <- paste0(pre, sep, seq_len(n))
  }
  if(all(name == "" | is.na(name))) {
    names(x) <- paste0(pre, sep, seq_len(n))
  } else {
    idx <- name == "" | is.na(name)
    name[idx] <- paste0(pre, sep, sum(idx))
    names(x) <- make.unique(name)
  }
  return(x)
}

#' @noRd
new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) stop("Elements must be named", call. = FALSE)
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) stop("Elements must equal the number of rows or 1", call. = FALSE)
    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- "data.frame"

  attr(x, "row.names") <- .set_row_names(n)
  x
}

#' @noRd
format_number <- function(x, digits = 2, nsmall = 2) {
  if(!is.numeric(x))
    stop("`x` must be a numeric vector.", call. = FALSE)
  x <- round(x, digits = digits)
  format(x, nsmall = nsmall)
}

#' @noRd
modify_list <- function (..., params, keep.null = TRUE)
{
  ll <- list(...)

  modifyList(ll, params, keep.null = keep.null)
}

#' @noRd
cat_line <- function(x, name = "", collapse = " ", sep = " ", width = 60) {
  msg <- paste(name, paste(x, collapse = collapse), sep = sep)
  n <- nchar(msg)
  if(n > width) {
    msg <- paste(strtrim(msg, width), "...", sep = " ")
  }
  cat(msg, sep = "\n")
}

#' @noRd
`%||%` <- function(x, y)
{
  if(is.null(x)) y else x
}

#' @noRd
.default_colors <- c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                     "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                     "#4393C3", "#2166AC", "#053061")
#' @noRd
rd_aesthetics <- function(type, name) {
  obj <- switch(type,
                geom = ggplot2:::check_subclass(name, "Geom", env = globalenv()),
                stat = ggplot2:::check_subclass(name, "Stat", env = globalenv())
  )
  aes <- rd_aesthetics_item(obj)

  c(
    "@section Aesthetics:",
    paste0(
      "\\code{", type, "_", name, "()} ",
      "understands the following aesthetics (required aesthetics are in bold):"
    ),
    "\\itemize{",
    paste0("  \\item ", aes),
    "}"
  )
}

#' @noRd
rd_aesthetics_item <- function(x) {
  req <- x$required_aes
  all <- union(req, sort(x$aesthetics()))

  ifelse(all %in% req,
         paste0("\\strong{\\code{", all, "}}"),
         paste0("\\code{", all, "}")
  )
}

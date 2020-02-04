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
sig_mark <- function(p.value,
                     sig.level = c(0.05, 0.01, 0.001),
                     mark = c("*", "**", "***")) {
  if(!is.numeric(p.value))
    p.value <- as.numeric(p.value)
  ord <- order(sig.level)
  sig.level <- sig.level[ord]
  mark <- mark[ord]
  brks <- c(0, sig.level, 1)
  lbs <- c(mark, "")
  pp <- cut(p.value, breaks = brks, labels = lbs, include.lowest = FALSE, right = TRUE)
  ifelse(p.value == 0, mark[1], as.character(pp))
}

#' @noRd
format_number <- function(x, digits = 2, nsmall = 2) {
  if(!is.numeric(x))
    stop("`x` must be a numeric vector.", call. = FALSE)
  x <- round(x, digits = digits)
  format(x, nsmall = nsmall)
}

#' @noRd
`%||%` <- function(x, y)
{
  if(is.null(x)) y else x
}

#' @noRd
utils::globalVariables(
  c(
    "r",
    "p.value",
    ".row.id",
    ".col.id"
  )
)

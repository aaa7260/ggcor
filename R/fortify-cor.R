
#' @export
fortify_cor <- function(
  x,
  y = NULL,
  type = "full",
  show.diag = FALSE,
  cor.test = FALSE,
  cluster = FALSE,
  cluster.method = "complete",
  ...
  )
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
  clss <- c("cor_tbl", "correlation", "rcorr", "corr.test", "mantel_tbl")
  if(any(clss %in% class(x))) {
    return(as_cor_tbl(x, type = type, show.diag = show.diag, cluster = cluster,
                      cluster.method = cluster.method, ...))
  }
  if(!(is.matrix(x) || is.data.frame(x)))
    stop("Need a matrix or data.frame.", call. = FALSE)
  corr <- correlate(x, y, cor.test, ...)
  as_cor_tbl(corr, type = type, show.diag = show.diag, cluster = cluster,
             cluster.method = cluster.method)
}

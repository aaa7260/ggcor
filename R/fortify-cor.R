
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
  if(inherits(x, "cor_tbl")) {
    return(x)
  }
  type <- match.arg(type, c("full", "upper", "lower"))
  if(!(is.matrix(x) || is.data.frame(x)))
    stop("Need a matrix or data.frame.", call. = FALSE)
  corr <- correlate(x, y, cor.test, ...)
  as_cor_tbl(corr, type = type, show.diag = show.diag, cluster = cluster,
             cluster.method = cluster.method)
}





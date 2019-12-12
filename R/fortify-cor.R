
#' @export
fortify_cor <- function(
  x,
  y = NULL,
  type = "full",
  show.diag = FALSE,
  cor.test = FALSE,
  cluster = FALSE,
  cluster.method = "complete",
  cluster.dist = 1 - x,
  ...
  )
{
  if(!(is.matrix(x) || is.data.frame(x)))
    stop("Need a matrix or data.frame.", call. = FALSE)
  type <- match.arg(type, c("full", "upper", "lower"))
  if(inherits(x, "cor_tbl")) {
    return(x)
  }
  corr <- correlate(x, y, cor.test, ...)
  as_cor_tbl(corr = corr, type = type, show.diag = show.diag, p = p, low = low,
            upp = upp, cluster = cluster, cluster.method = cluster.method,
            cluster.dist = cluster.dist)
}





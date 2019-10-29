
#' @export

fortify_cor <- function(
  x,
  y = NULL,
  type = c("full", "upper", "lower"),
  show.diag = FALSE,
  cor.test = FALSE,
  cor.test.alt = "two.sided",
  cor.test.method = "pearson",
  cluster.type = c("none", "all", "row", "col"),
  cluster.method = "HC",
  cluster.absolute = FALSE,
  keep.name = FALSE,
  ...   # pass to cor( )
  )
{
  if(!(is.matrix(x) || is.data.frame(x)))
    stop("Need a matrix or data.frame.", call. = FALSE)
  type <- match.arg(type)
  if(inherits(x, "cor_tbl")) {
    df <- x
  }
  x <- make_matrix_name(x)
  m <- cor(x, y = y, ...)
  p <- NULL
  low <- NULL
  upp <- NULL
  if(cor.test) {
    conf <- cor_test(x = x, y = y, alternative = cor.test.alt, method = cor.test.method)
    p <- conf$p
    low <- conf$low
    upp <- conf$upp
    if(cor.test.method != "pearson") {
      low <- NULL
      upp <- NULL
    }
  }
  as_cor_tbl(x = m, type = type, show.diag = show.diag, p = p, low = low,
            upp = upp, cluster.type = cluster.type, cluster.method = cluster.method,
            absolute = cluster.absolute, keep.name = keep.name)
}





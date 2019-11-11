#' @export
ggcor <- function(
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
  ...
)
{

  # handle data
  if(!is_cor_tbl(x) && !is_cor_tbl_fct(x) && !is_mantel_tbl(x)
     && !inherits(x, "rcorr") && !inherits(x, "corr.test")) {
    x <- fortify_cor(x = x,
                     y = y,
                     type = type,
                     show.diag = show.diag,
                     cor.test = cor.test,
                     cor.test.alt = cor.test.alt,
                     cor.test.method = cor.test.method,
                     cluster.type = cluster.type,
                     cluster.method = cluster.method,
                     cluster.absolute = cluster.absolute,
                     keep.name = keep.name)
  }
  ggcor_(x, keep.name = keep.name, ...)
}


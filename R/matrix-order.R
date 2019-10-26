#' @export
matrix_order <- function(x,
                         cluster.type   = c("none", "all", "row", "col"),
                         cluster.method = "HC",
                         absolute = FALSE,
                         ...)
{
  if(!is.matrix(x))
    x <- as.matrix(x)
  n <- nrow(x)
  m <- ncol(x)
  cluster.type <- match.arg(cluster.type)
  if(cluster.type == "none")
    return(list(row_ord = 1:n, col_ord = 1:m))
  if(absolute) x <- abs(x)
  if (cluster.method %in% c("BEA", "BEA_TSP", "PCA", "PCA_angle")) {
    row_ord <- seriation::seriate(x, method = cluster.method, ...)
    col_ord <- seriation::seriate(t(x), method = cluster.method, ...)
  } else {
    row_ord <- seriation::seriate(dist(x), method = cluster.method, ...)
    col_ord <- seriation::seriate(dist(t(x)), method = cluster.method, ...)
  }
  if(cluster.type == "all") {
    ord <- list(row_ord = seriation::get_order(row_ord),
                col_ord = seriation::get_order(col_ord))
  }
  if(cluster.type == "row") {
    ord <- list(row_ord = seriation::get_order(row_ord),
                col_ord = 1:m)
  }
  if(cluster.type == "col") {
    ord <- list(row_ord = 1:n,
                col_ord = seriation::get_order(col_ord))
  }
  ord
}

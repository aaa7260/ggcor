#' Reorder Matrices
#' @description  Tries to find an linear order for matrix by different cluster methods.
#' @param x a matrix object.
#' @param cluster.type a character string with the name of reorder type.
#' @param cluster.method a character string with the name of the seriation method.
#' @param absolute logical, if TRUE will transform x using abs().
#' @param ... extra params passing to \code{\link[seriation]{seriate}}.
#' @details All cluster methods in \code{\link[seriation]{seriate}} are supported，and the
#'     default cluster method is "HC". other methods please see `?seriation::seriate`.
#' @return a list of two numeric vector. The 'row_ord' is the order of rows, and
#'     the 'col_ord' is the order of columns.
#' @examples
#' m <- matrix(rnorm(20), nrow = 4)
#' matrix_order(m, cluster.type = "all")
#' @seealso \code{\link[seriation]{seriate}}.
#' @author Houyun Huang
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

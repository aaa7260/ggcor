#' Reorder Matrices
#' @description  Tries to find an linear order for matrix by different cluster methods.
#' @param x a matrix object.
#' @param cluster.method a character string with the name of agglomeration method.
#' @param cluster.dist function of trans correlation matrix to dist.
#' @param ... extra params passing to \code{\link[stats]{hclust}}.
#' @details Now it just supports for square matrix.
#' @return a numeric vector of new order..
#' @examples
#' m <- matrix(rnorm(25), nrow = 5)
#' matrix_order(m)
#' @seealso \code{\link[stats]{hclust}}.
#' @author Houyun Huang
#' @author Lei Zhou
#' @author Jian Chen
#' @author Taiyun Wei
#' @export
matrix_order <- function(x,
                         cluster.method = "complete",
                         cluster.dist = 1-x,
                         ...)
{
  if(!is.matrix(x))
    x <- as.matrix(x)
  d <- as.dist(eval(substitute(cluster.dist)))
  cluster <- hclust(d, cluster.method, ...)
  cluster$order
}

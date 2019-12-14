#' Reorder Matrices
#' @description  Tries to find an order for matrix by different cluster methods.
#' @param x a matrix object.
#' @param cluster.method a character string with the name of agglomeration method.
#' @param ... extra params passing to \code{\link[stats]{hclust}}.
#' @details Now it just supports for square matrix.
#' @return a numeric vector of new order.
#' @examples
#' m <- matrix(rnorm(25), nrow = 5)
#' matrix_order(m)
#' @seealso \code{\link[stats]{hclust}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
matrix_order <- function(x,
                         cluster.method = "complete",
                         ...)
{
  if(!is.matrix(x))
    x <- as.matrix(x)
  cluster <- hclust(as.dist(1 - x), cluster.method, ...)
  cluster$order
}

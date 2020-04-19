#' Reorder Matrices
#' @description  Tries to find an order for matrix by different cluster methods.
#' @param x a matrix-like object.
#' @param is.cor logical value (defaults to TRUE) indicating wheater
#' \code{x} is a correlation matrix.
#' @param k integer, the number of cluster group.
#' @param cluster.method a character string with the name of agglomeration method.
#' @param ... extra params passing to \code{\link[stats]{hclust}}.
#' @details Now it just supports for square matrix.
#' @return a list of hclust object.
#' @importFrom stats as.dist dist hclust
#' @rdname matrix_order
#' @examples
#' m <- matrix(rnorm(25), nrow = 5)
#' matrix_order(m)
#' @seealso \code{\link[stats]{hclust}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
matrix_order <- function(x,
                         is.cor = TRUE,
                         cluster.method = "complete",
                         ...)
{
  if(!is.matrix(x))
    x <- as.matrix(x)
  if(isTRUE(is.cor)) {
    if(!isSymmetric(x) || any(colnames(x) != rownames(x))) {
      row.cluster <- hclust(dist(x), cluster.method, ...)
      col.cluster <- hclust(dist(t(x)), cluster.method, ...)
    } else {
      row.cluster <- col.cluster <- hclust(as.dist(1 - x), cluster.method, ...)
    }
  } else {
    row.cluster <- hclust(dist(x))
    col.cluster <- hclust(dist(t(x)))
  }
  list(row.cluster = row.cluster,
       col.cluster = col.cluster)
}

#' @importFrom stats hclust cutree
#' @rdname matrix_order
#' @export
tidy_hc_rect <- function(x,
                         k = 2,
                         cluster.method = "complete",
                         ...)
{
  if(inherits(x, "hc_rect_df")) {
    return(x)
  }
  n <- nrow(x)
  tree <- hclust(as.dist(1 - x), cluster.method, ...)
  hc <- cutree(tree, k = k)
  clustab <- table(hc)[unique(hc[tree$order])]
  cu <- c(0, cumsum(clustab))

  structure(.Data = new_data_frame(
    list(xmin = cu[-(k + 1)] + 0.5,
         ymin = n - cu[-(k + 1)] + 0.5,
         xmax = cu[-1] + 0.5,
         ymax = n - cu[-1] + 0.5)),
    class = c("hc_rect_df", "data.frame"))
}

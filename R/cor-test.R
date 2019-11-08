#' Significance Test for Correlattion.
#' @description Significance test which produces p-values and confidence intervals for
#'     each pair of input features.
#' @param x a matrix object.
#' @param y a matrix object or NULL.
#' @param alternative indicates the alternative hypothesis and must be one of "two.sided",
#'     "greater" or "less".
#' @param method a character string indicating which correlation coefficient is to be used
#'     for the test. One of "pearson", "kendall", or "spearman".
#' @param ... extra params passing to `cor.test()`.
#' @details The columns of 'x' will be tested for each pair when y is NULL(the default),
#'     otherwise each column in 'x' and each column in 'y' is tested for each pair.
#' @return a list with P values matrix, upper of confidence intervals matrix and lower of
#'     confidence intervals matrix.
#' @importFrom stats cor.test
#' @examples
#' cor_test(mtcars)
#' m1 <- matrix(rnorm(100), nrow = 10)
#' m2 <- matrix(rnorm(60), nrow = 10)
#' cor_test(m1, m2)
#' @seealso \code{\link[stats]{cor.test}}.
#' @author Houyun Huang
#' @author Lei Zhou
#' @author Jian Chen
#' @author Taiyun Wei
#' @export
cor_test <- function(x,
                     y = NULL,
                     alternative = "two.sided",
                     method = "pearson",
                     ...) {
  x <- as.matrix(x)
  if(!is.null(y)) {
    y <- as.matrix(y)
  } else {
    y <- x
  }
  n <- ncol(x)
  m <- ncol(y)
  p <- low <- upp <- matrix(NA, ncol = m, nrow = n)
  for (i in 1:n) {
    for (j in 1:m) {
      suppressWarnings(
        tmp <- cor.test(x = x[ , i], y = y[ , j], alternative = alternative,
                                       method = method, ...)
        )
      p[i, j] <- tmp$p.value

      # only "pearson" method provides confidence intervals
      if (method == "pearson") {
        low[i, j] <- tmp$conf.int[1]
        upp[i, j] <- tmp$conf.int[2]
      }
    }
  }

  list(
    p = p,
    low = low,
    upp = upp
  )
}

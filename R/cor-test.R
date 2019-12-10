#' Significance Test for Correlattion.
#' @description Significance test which produces p-values and confidence intervals for
#'     each pair of input features.
#' @param x a matrix object.
#' @param y a matrix object or NULL.
#' @cor.test logical, if \code{TRUE} (default) will test for correlation.
#' @param method a character string indicating which correlation coefficient is to be used
#'     for the test. One of "pearson", "kendall", or "spearman".
#' @param use an optional character string giving a method for computing covariances in the presence of missing values.
#' @param ... extra params passing to `cor.test()`.
#' @details The columns of 'x' will be tested for each pair when y is NULL(the default),
#'     otherwise each column in 'x' and each column in 'y' is tested for each pair.
#' @return a list with correlation matrix, P values matrix, upper of confidence intervals matrix and lower of
#'     confidence intervals matrix.
#' @importFrom stats cor.test
#' @importFrom purrr walk2
#' @examples
#' cor_test(mtcars)
#' m1 <- matrix(rnorm(100), nrow = 10)
#' m2 <- matrix(rnorm(60), nrow = 10)
#' correlate(m1, m2)
#' @seealso \code{\link[stats]{cor.test}}.
#' @author Houyun Huang
#' @author Lei Zhou
#' @author Jian Chen
#' @author Taiyun Wei
#' @export
correlate <- function(x,
                 y = NULL,
                 cor.test = FALSE,
                 method = "pearson",
                 use = "everything",
                 ...) {
  y <- y %||% x
  if(!is.matrix(x))
    x <- as.matrix(x)
  if(!is.matrix(y))
    y <- as.matrix(y)
  n <- ncol(x)
  m <- ncol(y)
  r <- cor(x, y, use = use, method = method)
  p.value <- lower.ci <- upper.ci <- matrix(NA, ncol = m, nrow = n)
  if(cor.test) {
    df <- expand.grid(1:n, 1:m)
    purrr::walk2(df$Var1, df$Var2, function(.idx, .idy) {
      tmp <- cor.test(x = x[ , .idx], y = y[ , .idy], method = method, ...)
      p.value[.idx, .idy] <<- tmp$p.value
      if(method == "pearson") {
        if (nrow(x) > 3) {
          lower.ci[.idx, .idy] <<- tmp$conf.int[1]
          upper.ci[.idx, .idy] <<- tmp$conf.int[2]
        } else {
          warning("correlation test interval at least needs 4 observations.", call. = FALSE)
        }
      }
    })
  }
  structure(
    .Data = list(
      r = r,
      p.value = p.value,
      lower.ci = lower.ci,
      upper.ci = upper.ci
    ), class = "correlation"
  )
}

#' @noRd
print.correlation <- function(x, all = FALSE, ...) {
  if(all) print(x, ...) else print(x$r, ...)
}

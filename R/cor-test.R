#' Matrix of Correlations, P-values and confidence intervals
#' @description \code{correlate} uses \code{cor} to find the correlations and use \code{cor.test} to find
#'     the p values, confidence intervals for all possible pairs of columns ofmatrix.
#' @param x, y a matrix object or NULL.
#' @param cor.test logical, if \code{TRUE} (default) will test for correlation.
#' @param method a character string indicating which correlation coefficient is to be used
#'     for the test. One of "pearson", "kendall", or "spearman".
#' @param use an optional character string giving a method for computing covariances in the presence of missing values.
#' @param ... extra params passing to \code{cor.test}.
#' @details The columns of 'x' will be tested for each pair when y is NULL(the default),
#'     otherwise each column in 'x' and each column in 'y' is tested for each pair.
#' @return a list with correlation matrix, P values matrix, confidence intervals matrix.
#' @importFrom stats cor cor.test
#' @importFrom purrr walk2
#' @examples
#' cor_test(mtcars)
#' m1 <- matrix(rnorm(100), nrow = 10)
#' m2 <- matrix(rnorm(60), nrow = 10)
#' correlate(m1, m2)
#' @seealso \code{\link[stats]{cor}}, \code{\link[stats]{cor.test}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
correlate <- function(x,
                      y = NULL,
                      cor.test = FALSE,
                      method = "pearson",
                      use = "everything",
                      ...)
{
  y <- y %||% x
  if(!is.matrix(x))
    x <- as.matrix(x)
  if(!is.matrix(y))
    y <- as.matrix(y)
  n <- ncol(x)
  m <- ncol(y)
  r <- cor(x, y, use = use, method = method)
  if(cor.test) {
    p.value <- lower.ci <- upper.ci <- matrix(NA, ncol = m, nrow = n)
    df <- expand.grid(1:n, 1:m)
    purrr::walk2(df$Var1, df$Var2, function(.idx, .idy) {
      tmp <- cor.test(x = x[ , .idx], y = y[ , .idy], method = method, ...)
      p.value[.idx, .idy] <<- tmp$p.value
      if(method == "pearson") {
        if (nrow(x) > 3) {
          lower.ci[.idx, .idy] <<- tmp$conf.int[1]
          upper.ci[.idx, .idy] <<- tmp$conf.int[2]
        } else {
          warning("correlation test interval needs 4 observations at least.", call. = FALSE)
        }
      }
    })
  }
  if(cor.test) {
    lower.ci <- if(method == "pearson") lower.ci else NULL
    upper.ci <- if(method == "pearson") upper.ci else NULL
  } else {
    p.value <- lower.ci <- upper.ci <- NULL
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

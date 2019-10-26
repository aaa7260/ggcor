#' @noRd
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

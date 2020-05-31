#' Relative importance
#' @title Relative importance
#' @param spec,env a data.frame object.
#' @param type one of "lmg", "last", "first", "betasq", "pratt", "genizi" or "car".
#' @param byrow a logical value, if TRUE, the 'spec' on the rows.
#' @param x a calc_relimp object.
#' @param ... extra parameters.
#' @return a calc_relimp object.
#' @rdname calc_relimp
#' @examples \dontrun{
#' spec <- mtcars[c(1, 3, 4, 5)]
#' env <- mtcars[6:11]
#' calc_relimp(spec, env)
#' }
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
calc_relimp <- function(spec,
                        env,
                        type = "lmg",
                        byrow = FALSE,
                        ...)
{
  type <- match.arg(type, c("lmg", "last", "first", "betasq", "pratt", "genizi", "car"))
  if(!is.data.frame(spec))
    spec <- as.data.frame(spec)
  if(!is.data.frame(env))
    env <- as.data.frame(env)

  n <- length(spec)
  m <- length(env)
  if(any(n < 1, m < 1)) {
    stop("Zero length data.", call. = FALSE)
  }
  calc.relimp <- get_function("relaimpo", "calc.relimp")

  explained <- structure(.Data = vector(length = n), names = names(spec), class = "numeric")
  if(byrow) {
    importance <- matrix(NA, nrow = n, ncol = m, dimnames = list(names(spec), names(env)))
  } else {
    importance <- matrix(NA, nrow = m, ncol = n, dimnames = list(names(env), names(spec)))
  }
  for (i in seq_len(n)) {
    cr <- calc.relimp(spec[[i]] ~ ., data = env, importance = TRUE, ...)
    explained[i] <- extract_s4(cr, "R2") * 100
    if(isTRUE(byrow)) {
      importance[i, ] <- extract_s4(cr, type)
    } else {
      importance[, i] <- extract_s4(cr, type)
    }
  }
  structure(.Data = list(explained = as.data.frame(explained),
                         importance = as.data.frame(importance)),
            class = "calc_relimp")
}

#' @rdname calc_relimp
print.calc_relimp <- function(x, ...) {
  cat("Var explained (%):\n")
  print(x$explained)
  cat("\n")
  cat("Var importance:\n")
  print(x$importance)
}

#' @noRd
extract_s4 <- function(x, e) {
  do.call("@", list(x, e))
}

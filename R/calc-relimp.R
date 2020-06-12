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
#' @export
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

  if(nrow(spec) != nrow(env)) {
    stop("'env' shold have the same rows as 'spec'.", call. = FALSE)
  }

  if(!all(vapply(spec, is.numeric, logical(1))) &&
     !all(vapply(env, is.numeric, logical(1)))) {
    stop("Only support for numeric variable.", call. = FALSE)
  }

  calc.relimp <- get_function("relaimpo", "calc.relimp")
  explained <- vector(length = n)
  importance <- matrix(NA, nrow = n, ncol = m, dimnames = list(names(spec), names(env)))
  p.value <- matrix(NA, nrow = n, ncol = m, dimnames = list(names(spec), names(env)))

  for (i in seq_len(n)) {
    lm <- stats::lm(spec[[i]] ~ ., data = env)

    sm <- summary(lm)
    cr <- calc.relimp(lm, type = type, ...)
    explained[i] <- extract_s4(cr, "R2") * 100
    importance[i, ] <- extract_s4(cr, type)
    p.value[i, ] <- sm$coefficients[, "Pr(>|t|)"][-1]
  }

  if(isFALSE(byrow)) {
    importance <- t(importance)
    p.value <- t(importance)
  }

  structure(.Data = list(explained = data.frame(name = names(spec),
                                                explained = explained,
                                                stringsAsFactors = FALSE),
                         importance = as.data.frame(importance),
                         p.value = p.value),
            class = "calc_relimp")
}

#' @rdname calc_relimp
#' @export
print.calc_relimp <- function(x, ...) {
  cat("Var explained (%):\n")
  print(x$explained)
  cat("\n")
  cat("Var importance:\n")
  print(x$importance)
  cat("\n")
  cat("Var importance p value:\n")
  print(x$p.value)
}

#' @noRd
extract_s4 <- function(x, e) {
  do.call("@", list(x, e))
}

#' Relative importance
#' @title Relative importance
#' @param spec,env a data.frame object.
#' @param family family function.
#' @param type one of "lmg", "last", "first", "betasq", "pratt", "genizi" or "car".
#' @param byrow a logical value, if TRUE, the 'spec' on the rows.
#' @param x a calc_relimp object.
#' @param ... extra parameters.
#' @return a calc_relimp object.
#' @importFrom stats lm glm binomial
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
                        family = binomial(link = "logit"),
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

  sid <- vapply(spec, is.numeric, logical(1))

  calc.relimp <- get_function("relaimpo", "calc.relimp")
  explained <- structure(.Data = vector(length = n), names = names(spec), class = "numeric")
  if(byrow) {
    importance <- matrix(NA, nrow = n, ncol = m, dimnames = list(names(spec), names(env)))
  } else {
    importance <- matrix(NA, nrow = m, ncol = n, dimnames = list(names(env), names(spec)))
  }
  if(byrow) {
    p.value <- matrix(NA, nrow = n, ncol = m, dimnames = list(names(spec), names(env)))
  } else {
    p.value <- matrix(NA, nrow = m, ncol = n, dimnames = list(names(env), names(spec)))
  }
  for (i in seq_len(n)) {
    lm <- if(sid[i]) {
      stats::lm(spec[[i]] ~ ., data = env)
    } else {
      stats::glm(spec[[i]] ~ ., data = env, family = family)
    }
    sm <- summary(lm)
    cr <- calc.relimp(lm, type = type, ...)
    explained[i] <- extract_s4(cr, "R2") * 100
    if(isTRUE(byrow)) {
      importance[i, ] <- extract_s4(cr, type)
      p.value[i, ] <- sm$coefficients[, "Pr(>|t|)"][-1]
    } else {
      importance[, i] <- extract_s4(cr, type)
      p.value[, i] <- sm$coefficients[, "Pr(>|t|)"][-1]
    }
  }
  structure(.Data = list(explained = as.data.frame(explained),
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

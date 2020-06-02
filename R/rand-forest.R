#' Random forests
#' @title Random forests
#' @param spec,env a data.frame object.
#' @param byrow a logical value, if TRUE, the 'spec' on the rows.
#' @param seed a integer value.
#' @param x a rand_forest object.
#' @param ... extra parameters.
#' @return a rand_forest object.
#' @rdname rand_forest
#' @examples \dontrun{
#' spec <- mtcars[c(1, 3, 4, 5)]
#' env <- mtcars[6:11]
#' rand_forest(spec, env)
#' }
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
rand_forest <- function(spec,
                        env,
                        byrow = FALSE,
                        seed = 123,
                        ...)
{

  if(!is.data.frame(spec))
    spec <- as.data.frame(spec)
  if(!is.data.frame(env))
    env <- as.data.frame(env)

  n <- length(spec)
  m <- length(env)
  if(any(n < 1, m < 1)) {
    stop("Zero length data.", call. = FALSE)
  }
  rfPermute <- get_function("rfPermute", "rfPermute")
  rp.importance <- get_function("rfPermute", "rp.importance")
  set.seed(seed)
  seeds <- as.integer(stats::runif(n) * 10000)

  explained <- vector(length = n)
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
    set.seed(seeds[i])
    rf <- rfPermute(spec[[i]] ~ ., data = env, importance = TRUE, ...)
    type <- rf$type
    imp <- rp.importance(rf, scale = TRUE)
    if(type == "classification") {
      explained[i] <- 100 - 100 * rf$err.rate[rf$ntree, "OOB"]
    } else {
      explained[i] <- 100 * rf$rsq[length(rf$rsq)]
    }
    if(isTRUE(byrow)) {
      if(type == "classification") {
        importance[i, ] <- imp[, "MeanDecreaseAccuracy"]
        p.value[i, ] <- imp[, "MeanDecreaseAccuracy.pval"]
      } else {
        importance[i, ] <- imp[, "%IncMSE"]
        p.value[i, ] <- imp[, "%IncMSE.pval"]
      }
    } else {
      if(type == "classification") {
        importance[, i] <- imp[, "MeanDecreaseAccuracy"]
        p.value[, i] <- imp[, "MeanDecreaseAccuracy.pval"]
      } else {
        importance[, i] <- imp[, "%IncMSE"]
        p.value[, i] <- imp[, "%IncMSE.pval"]
      }
    }
  }
  structure(.Data = list(explained = data.frame(name = names(spec),
                                                explained = explained,
                                                stringsAsFactors = FALSE),
                         importance = as.data.frame(importance),
                         p.value = p.value),
            class = "rand_forest")
}

#' @rdname rand_forest
#' @export
print.rand_forest <- function(x, ...) {
  cat("Var explained (%):\n")
  print(x$explained)
  cat("\n")
  cat("Var importance:\n")
  print(x$importance)
  cat("\n")
  cat("Var importance p value:\n")
  print(x$p.value)
}

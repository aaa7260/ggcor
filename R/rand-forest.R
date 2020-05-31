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
  randomForest <- get_function("randomForest", "randomForest")
  importance <- get_function("randomForest", "importance")
  set.seed(seed)
  seeds <- as.integer(stats::runif(n) * 10000)

  explained <- structure(.Data = vector(length = n), names = names(spec), class = "numeric")
  if(byrow) {
    importance <- matrix(NA, nrow = n, ncol = m, dimnames = list(names(spec), names(env)))
  } else {
    importance <- matrix(NA, nrow = m, ncol = n, dimnames = list(names(env), names(spec)))
  }
  for (i in seq_len(n)) {
    set.seed(seeds[i])
    rf <- randomForest(spec[[i]] ~ ., data = env, importance = TRUE, ...)
    type <- rf$type
    if(type == "classification") {
      explained[i] <- 100 - 100 * rf$err.rate[rf$ntree, "OOB"]
    } else {
      explained[i] <- 100 * rf$rsq[length(rf$rsq)]
    }
    if(isTRUE(byrow)) {
      if(type == "classification") {
        importance[i, ] <- importance(rf)[, "MeanDecreaseAccuracy"]
      } else {
        importance[i, ] <- importance(rf)[, "%IncMSE"]
      }
    } else {
      if(type == "classification") {
        importance[, i] <- importance(rf)[, "MeanDecreaseAccuracy"]
      } else {
        importance[, i] <- importance(rf)[, "%IncMSE"]
      }
    }
  }
  structure(.Data = list(spec = spec,
                         env = env,
                         explained = as.data.frame(explained),
                         importance = as.data.frame(importance)),
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
}

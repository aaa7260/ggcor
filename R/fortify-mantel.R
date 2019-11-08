#' @export
#' @importFrom vegan vegdist
#' @importFrom stats dist
fortify_mantel <- function(spec,
                           env,
                           env.ctrl = NULL,
                           mantel.fun = "mantel",
                           is.pair = FALSE,
                           spec.select = NULL, # a list of index vector
                           spec.group = NULL,
                           env.group = NULL,
                           env.ctrl.group = NULL,
                           spec.dist.fun = "vegdist",
                           env.dist.fun = "vegdist",
                           spec.dist.method = "bray",
                           env.dist.method = "euclidean",
                           ...)
{
  data <- handle_mantel_data(
    spec = spec,
    env  = env,
    env.ctrl = env.ctrl,
    mantel.fun = mantel.fun,
    is.pair = is.pair,
    spec.select = spec.select, # a list of index vector
    spec.group = spec.group,
    env.group = env.group,
    env.ctrl.group = env.ctrl.group
  )
  res <- mantel_test(spec.list        = data$spec,
                     env.list         = data$env,
                     env.ctrl.list    = data$env.ctrl,
                     spec.dist.fun    = spec.dist.fun,
                     env.dist.fun     = env.dist.fun,
                     mantel.fun       = mantel.fun,
                     spec.dist.method = spec.dist.method,
                     env.dist.method  = env.dist.method,
                     ...)
  res
}

#' @export
mantel_test <- function(spec.list,
                        env.list,
                        env.ctrl.list = NULL,
                        spec.dist.fun = "vegdist",
                        env.dist.fun = "vegdist",
                        mantel.fun = "mantel",
                        spec.dist.method = "bray",
                        env.dist.method = "euclidean",
                        ...
)
{
  spec.list <- make_list_names(spec.list, "spec")
  env.list <- make_list_names(env.list, "env")
  spec.name <- names(spec.list)
  env.name <- names(env.list)
  n <- length(spec.list)
  m <- length(env.list)
  if(n != m) {
    stop("'spec.list' and 'env.list' must be same length.", call. = FALSE)
  }
  if(mantel.fun == "mantel.partial") {
    if(is.null(env.ctrl.list))
      stop("Did you forget to set the 'env.ctrl.list' params?", call. = FALSE)
    if(length(env.list) != length(env.ctrl.list))
      env.ctrl.list <- rep_len(env.ctrl.list, m)
  }
  spec.dist <- lapply(spec.list, function(x) {
    do.call(spec.dist.fun, list(x = x, method = spec.dist.method))
  })
  env.dist <- lapply(env.list, function(x) {
    do.call(env.dist.fun, list(x = x, method = env.dist.method))
  })
  if(mantel.fun == "mantel.partial" && !is.null(env.ctrl)) {
    env.ctrl.dist <- lapply(env.ctrl.list, function(x) {
      do.call(env.dist.fun, list(x = x, method = env.dist.method))
    })
  }
  res <- switch (mantel.fun,
                 mantel.partial  = purrr::pmap(list(spec.dist, env.dist, env.ctrl.dist), vegan::mantel.partial, ...),
                 mantel          = purrr::map2(spec.dist, env.dist, vegan::mantel, ...),
                 mantel.randtest = purrr::map2(spec.dist, env.dist, ade4::mantel.randtest, ...),
                 mantel.rtest = purrr::map2(spec.dist, env.dist, ade4::mantel.rtest, ...)
  )
  rp <- extract_mantel(res, mantel.fun)
  out <- tibble::tibble(spec = spec.name,
                        env = env.name,
                        r = rp$r,
                        p = rp$p)
  class(out) <- c("mantel_tbl", class(out))
  out
}

#' @export
is_mantel_tbl <- function(x) {
  inherits(x, "mantel_tbl")
}

#' @noRd
handle_mantel_data <- function(spec, # df
                               env, # df
                               env.ctrl = NULL, # df
                               mantel.fun = "mantel",
                               is.pair = FALSE,
                               spec.select = NULL, # a list of index vector
                               spec.group = NULL, # the same length vector with spec
                               env.group = NULL, # the same length vector with env
                               env.ctrl.group = NULL) # the same length vector with env
{
  if(!is.data.frame(spec) && !is.list(spec))
    spec <- as.data.frame(spec)
  if(!is.data.frame(env) && !is.list(env))
    env <- as.data.frame(env)
  env.col <- length(env)
  env.colname <- names(env)
  if(!is.null(spec.select)) {
    if(!is.null(spec.group)) {
      warning("Just supports for 'spec.select' or 'spec.group', 'spec.group' is droped", call. = FALSE)
      spec.group <- NULL
    }
  }
  if(!is.null(spec.select))
    spec <- lapply(spec.select, function(x) {
      subset(spec, select = x, drop = FALSE)
    })
  if(!is.null(spec.group)) {
    spec <- split(spec, spec.group, drop = FALSE)
  }

  if(!is.null(env.group)) {
    env <- split(env, env.group, drop = FALSE)
  }
  if(mantel.fun == "mantel.partial") {
    if(is.null(env.ctrl))
      stop("Did you forget to set the 'env.ctrl.list' params?", call. = FALSE)
    if(!is.data.frame(env.ctrl) && !is.list(env.ctrl))
      env.ctrl <- as.data.frame(env.ctrl)
    if(!is.null(env.ctrl.group)) {
      env.ctrl <- split(env.ctrl, env.ctrl.group, drop = FALSE)
    }
    if(is.data.frame(env.ctrl)) {
      env.ctrl <- list(env.ctrl)
    }
    if(length(env.ctrl) != length(env))
      env.ctrl <- rep_len(env.ctrl, length(env))
  }
  spec <- make_list_names(spec)
  env <- make_list_names(env)
  spec.name <- names(spec)
  env.name <- names(env)
  n <- length(spec)
  m <- if(is.pair) env.col else length(env)
  spec <- rep(spec, m)
  names(spec) <- rep(spec.name, m)
  if(is.pair) {
    env <- flatten_list(env)
    names(env) <- rep(env.colname, each = n)
  } else {
    env <- rep(env, each = n)
    names(env) <- rep(env.name, each = n)
  }
  if(mantel.fun == "mantel.partial") {
    env.ctrl <- rep(env.ctrl, each = n)
  }
  list(spec = spec, env = env, env.ctrl = env.ctrl)
}

#' @noRd
flatten_list <- function(x) {
  if(!is.list(x))
    return(x)
  nn <- purrr::map_dbl(x, function(e) {if(!is.list(e)) 1 else length(e)})
  cnn <- cumsum(nn)
  ll <- vector("list", sum(nn))
  for(i in seq_len(length(x))) {
    if(i == 1) {
      if(is.list(x[[i]])) {
        ll[1:cnn[1]] <- x[[1]][1:nn[1]]
      } else {
        ll[1] <- x[1]
      }
    } else {
      idx <- (cnn[i - 1] + 1): cnn[i]
      if(is.list(x[[i]])) {
        ll[idx] <- x[[i]][1:nn[i]]
      } else {
        ll[cnn[i]] <- x[i]
      }
    }
  }
  ll
}

#' @noRd
extract_mantel <- function(x,
                           FUN = c("mantel", "mantel.partial",
                                   "mantel.randtest", "mantel.rtest")) {
  FUN <- match.arg(FUN)
  if(FUN %in% c("mantel", "mantel.partial")) {
    res <- purrr::map(x, function(m) c(r = m$statistic, p = m$signif))
  } else {
    res <- purrr::map(x, function(m) c(r = m$obs, p = m$pvalue))
  }
  df <- tibble::tibble(r = purrr::map_dbl(res, `[[`, "r"),
                       p = purrr::map_dbl(res, `[[`, "p"))
  df
}



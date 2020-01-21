#' Tidy for mantel tests
#' @description Enhanced encapsulation of the mantel_test function.
#' @param spec,env data frame object.
#' @param group vector for rows grouping.
#' @param env.ctrl NULL (default), data frame or named list of data frame.
#' @param mantel.fun string, function of mantel test.
#' @param ... extra params for \code{\link[ggcor]{mantel_test}}.
#' @importFrom dplyr %>% mutate
#' @importFrom purrr pmap_dfr
#' @rdname fortify_mantel
#' @examples
#' library(vegan)
#' data("varespec")
#' data("varechem")
#' fortify_mantel(varespec, varechem,
#'   spec.select = list(spec01 = 1:5, spec02 = 6:12))
#' fortify_mantel(varespec, varechem,
#'   spec.select = list(spec01 = 1:5, spec02 = 6:12),
#'   env.select = list(env01 = 1:5, env02 = 6:10, env03 = 11:14))
#' set.seed(20191224)
#' sam_grp <- sample(paste0("sample", 1:3), 24, replace = TRUE)
#' fortify_mantel(varespec, varechem, group = sam_grp)
#' @seealso \code{\link[ggcor]{mantel_test}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
fortify_mantel <- function(spec,
                           env,
                           group = NULL,
                           env.ctrl = NULL, # named list if grouped
                           mantel.fun = "mantel",
                           ...)
{
  if(!is.data.frame(spec))
    spec <- as.data.frame(spec)
  if(!is.data.frame(env))
    env <- as.data.frame(env)
  if(nrow(spec) != nrow(env)) {
    stop("'spec' must have the same rows as 'env'.", call. = FALSE)
  }
  if(mantel.fun == "mantel.partial") {
    if(is.null(env.ctrl))
      stop("Did you forget to set the 'env.ctrl' param?", call. = FALSE)
    if(!is.data.frame(env.ctrl) && !is.list(env.ctrl))
      stop("'env.ctrl' needs a list or data.frame.", call. = FALSE)
  }
  if(!is.null(group)) {
    if(length(group) != nrow(spec))
      stop("Length of 'group' and rows of 'spec' must be same.", call. = FALSE)
    spec <- split(spec, group, drop = FALSE)
    env <- split(env, group, drop = FALSE)
    if(mantel.fun == "mantel.partial") {
      if(is.data.frame(env.ctrl)) {
        env.ctrl <- rep_len(list(env.ctrl), length(names(spec)))
      } else {
        env.ctrl <- env.ctrl[names(spec)]
      }
    } else {
      env.ctrl <- as.list(rep(NA, length(names(spec))))
    }
    df <- suppressMessages(
      purrr::pmap_dfr(list(spec, env, env.ctrl, as.list(names(spec))),
                      function(.spec, .env, .env.ctrl, .group) {
                        mantel_test(.spec, .env, .env.ctrl, mantel.fun, ...) %>%
                          dplyr::mutate(.group = .group)
                      })
    )
  } else {
    df <- mantel_test(spec, env, env.ctrl, mantel.fun, ...)
  }
  grouped <- if(!is.null(group)) TRUE else FALSE
  attr(df, "grouped") <- grouped
  df
}

#' Mantel and partial mantel test for dissimilarity matrices
#' @description Perform mantel test quickly and tidy up the data to
#'     data frame.
#' @param spec,env data frame object.
#' @param env.ctrl NULL (default), data frame.
#' @param mantel.fun string, function of mantel test.
#'    \itemize{
#'      \item{\code{"mantel"} will use \code{vegan::mantel} (default).}
#'      \item{\code{"mantel.randtest"} will use \code{ade4::mantel.randtest}.}
#'      \item{\code{"mantel.rtest"} will use \code{ade4::mantel.rtest}.}
#'      \item{\code{"mantel.partial"} will use \code{vegan::mantel.partial} (default).}
#'   }
#' @param spec.select,env.select NULL (default), numeric or character vector index of columns.
#' @param spec.dist.method dissimilarity index (default is 'bray'), pass to \code{method}
#'     params of \code{\link[vegan]{vegdist}}.
#' @param env.dist.method dissimilarity index (default is euclidean'), pass to \code{method}
#'     params of \code{\link[vegan]{vegdist}}.
#' @param ... extra params for \code{mantel.fun}.
#' @importFrom vegan vegdist mantel mantel.partial
#' @importFrom ade4 mantel.randtest mantel.rtest
#' @importFrom dplyr %>%
#' @importFrom purrr map map2
#' @rdname mantel_test
#' @examples
#' library(vegan)
#' data("varespec")
#' data("varechem")
#' mantel_test(varespec, varechem)
#' mantel_test(varespec, varechem, mantel.fun = "mantel.randtest")
#' mantel_test(varespec, varechem, mantel.fun = "mantel.randtest",
#'   spec.select = list(spec01 = 1:6, spec02 = 7:12))
#' mantel_test(varespec, varechem, mantel.fun = "mantel.randtest",
#'   spec.select = list(spec01 = 1:6, spec02 = 7:12),
#'   env.select = list(env01 = 1:4, env02 = 5:14))
#' nm <- names(varechem[1:9])
#' mantel_test(varespec, varechem, env.ctrl = varechem[10:14],
#'   mantel.fun = "mantel.partial",
#'   env.select = as.list(setNames(nm, nm)))
#' @seealso \code{\link[vegan]{vegdist}}, \code{\link[vegan]{mantel}},
#'     \code{\link[ade4]{mantel.rtest}}, \code{\link[ade4]{mantel.randtest}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
mantel_test <- function(spec,
                        env,
                        env.ctrl = NULL, # named list if grouped
                        mantel.fun = "mantel",
                        spec.select = NULL, # a list of index vector
                        env.select = NULL,
                        spec.dist.method = "bray",
                        env.dist.method = "euclidean",
                        ...)
{
  if(!is.data.frame(spec))
    spec <- as.data.frame(spec)
  if(!is.data.frame(env))
    env <- as.data.frame(env)
  if(nrow(spec) != nrow(env)) {
    stop("'spec' must have the same rows as 'env'.", call. = FALSE)
  }
  if(mantel.fun == "mantel.partial") {
    if(is.null(env.ctrl))
      stop("Did you forget to set the 'env.ctrl.list' param?", call. = FALSE)
    if(!is.data.frame(env.ctrl))
      env.ctrl <- as.data.frame(env.ctrl)
  }
  if(!is.list(spec.select) && !is.null(spec.select))
    stop("'spec.select' needs a list or NULL.", call. = FALSE)
  if(!is.list(env.select) && !is.null(env.select))
    stop("'env.select' needs a list or NULL.", call. = FALSE)
  if(is.null(spec.select)) {
    spec.select <- list(spec = 1:ncol(spec))
  }
  if(is.null(env.select)) {
    env.select <- as.list(setNames(1:ncol(env), names(env)))
  }
  spec.select <- make_list_names(spec.select, "spec")
  env.select <- make_list_names(env.select, "env")
  spec.name <- rep(names(spec.select), each = length(env.select))
  env.name <- rep(names(env.select), length(spec.select))
  spec <- purrr::map(spec.select, function(.x) {
    subset(spec, select = .x, drop = FALSE)})
  env <- purrr::map(env.select, function(.x) {
    subset(env, select = .x, drop = FALSE)})

  rp <- purrr::map2(spec.name, env.name, function(.x, .y) {
    spec.dist <- vegan::vegdist(spec[[.x]], method = spec.dist.method)
    env.dist <- vegan::vegdist(env[[.y]], method = env.dist.method)
    if(mantel.fun == "mantel.partial") {
      env.ctrl.dist <- vegan::vegdist(env.ctrl, method = env.dist.method)
    }
    switch (mantel.fun,
            mantel.partial  = vegan::mantel.partial(spec.dist, env.dist, env.ctrl.dist, ...),
            mantel          = vegan::mantel(spec.dist, env.dist, ...),
            mantel.randtest = ade4::mantel.randtest(spec.dist, env.dist, ...),
            mantel.rtest    = ade4::mantel.rtest(spec.dist, env.dist, ...),
    )
  }) %>% extract_mantel(mantel.fun)
  df <-
    structure(.Data = tibble::tibble(spec = spec.name,
                                     env = env.name,
                                     r = rp$r,
                                     p.value = rp$p.value),
              grouped = FALSE,
              class = c("mantel_tbl", "tbl_df", "tbl", "data.frame"))
}

#' @noRd
extract_mantel <- function(x, .f = "mantel") {
  .f <- match.arg(.f, c("mantel", "mantel.partial",
                        "mantel.randtest", "mantel.rtest"))
  if(.f %in% c("mantel", "mantel.partial")) {
    r <- purrr::map_dbl(x, `[[`, "statistic")
    p.value <- purrr::map_dbl(x, `[[`, "signif")
  } else {
    r <- purrr::map_dbl(x, `[[`, "obs")
    p.value <- purrr::map_dbl(x, `[[`, "pvalue")
  }
  list(r = r, p.value = p.value)
}

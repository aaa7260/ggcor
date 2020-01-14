#' Tidy co-occurrence network data
#' @description The function calculates correlation coefficient, statistical
#'     significance level and filters according to conditions.
#' @param x any \code{R} object can be converted to cor_tbl.
#' @param r.thres a numeric value.
#' @param r.absolute logical value (defaults to TRUE).
#' @param p.thres a numeric value.
#' @param ... passing to \code{\link[ggcor]{as_cor_tbl}}
#' @return cor_tbl.
#' @rdname create_network
#' @export
create_network <- function(x,
                           r.thres = 0.6,
                           r.absolute = TRUE,
                           p.thres = 0.05,
                           ...)
{
  df <- fortify_cor(x, type = "upper", show.diag = FALSE, cor.test = TRUE, ...)
  if(r.absolute) {
    df <- with(df, subset(df, abs(r) > r.thres, p.value < p.thres))
  } else {
    df <- with(df, subset(df, r > r.thres, p.value < p.thres))
  }
  df
}

#' @importFrom tibble tibble
#' @rdname create_network
#' @export
fast_create_network <- function(x,
                                r.thres = 0.6,
                                r.absolute = TRUE,
                                p.thres = 0.05,
                                ...)
{
  if(!requireNamespace("WGCNA", quietly = TRUE)) {
    stop("'fast_correlate' needs 'WGCNA' package.", call. = FALSE)
  }
  if(!is.data.frame(x))
    x <- as.data.frame(x)
  name <- names(x)
  corr <- WGCNA::corAndPvalue(x, ...)
  idx <- upper.tri(corr$cor)
  df <- tibble::tibble(.row.names = rep(name, length(name)),
                       .col.names = rep(name, each = length(name)),
                       r = as.vector(corr$cor),
                       p.value = as.vector(corr$p)) %>% subset(idx)
  if(r.absolute) {
    with(df, subset(df, abs(r) > r.thres, p.value < p.thres))
  } else {
    with(df, subset(df, r > r.thres, p.value < p.thres))
  }
}

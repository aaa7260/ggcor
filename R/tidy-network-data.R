#' Tidy co-occurrence network data
#' @description The function calculates correlation coefficient, statistical
#'     significance level and filters according to conditions.
#' @param x any \code{R} object can be converted to cor_tbl.
#' @param r.thres a numeric value.
#' @param r.absolute logical value (defaults to TRUE).
#' @param p.thres a numeric value.
#' @param ... passing to \code{\link[ggcor]{as_cor_tbl}}
#' @return cor_tbl.
#' @rdname tidy_network_data
#' @export
tidy_network_data <- function(x,
                              r.thres = 0.6,
                              r.absolute = TRUE,
                              p.thres = NULL,
                              type = "upper",
                              show.diag = FALSE,
                              cor.test = TRUE,
                              ...)
{
  df <- fortify_cor(x, type = type, show.diag = show.diag,
                    cor.test = cor.test, ...)
  if(r.absolute) {
    df <- with(df, subset(df, abs(r) > r.thres))
  } else {
    df <- with(df, subset(df, r > r.thres))
  }
  if(!is.null(p.thres)) {
    df <- with(df, subset(df, p.value < p.thres))
  }
  df
}

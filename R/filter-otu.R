#' Filter OTU table by relative abundance.
#' @description Lower OTUs is filtered according to relative abundance to
#'    reduce the dimension of OTU table.
#' @param x a OTU table, every otu on columns.
#' @param thres a numeric value, OTUs less than this value are filtered.
#' @param trans.frac logical value indicates whether to scale the original
#'     OTU table to relative abundance.
#' @return new OTU table.
#' @rdname filter_otu
#' @export
filter_otu <- function(x, thres = 10^-5, trans.frac = FALSE) {
  if(!is.data.frame(x))
    x <- as.data.frame(x)
  row_sum <- apply(x, 1, sum)
  if(trans.frac) {
    x <- x / row_sum
  } else {
    x_frac <- x / row_sum
    col_sum <- apply(x_frac, 2, sum)
  }
  return(x[col_sum > thres])
}

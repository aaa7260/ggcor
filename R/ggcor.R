#' Create a correlation plot
#' @description This function is the equivalent of \code{\link[ggplot2]{ggplot}}
#' in ggplot2. It takes care of setting up the position of axis and legend for
#' the plot based on the plot type.
#' @param data cor_tbl object.
#' @param mapping NULL (default) or a list of aesthetic mappings to use for plot.
#' @param axis.x.position,axis.y.position the position of the axis. 'auto' (default)
#' is set according to the plot type, 'bottom' or 'top' for x axes, 'left' or 'right'
#' for y axes.
#' @param drop logical value, if TRUE (default) will drop the unused factor levels.
#' @param ... extra parameters.
#' @return an object of class gg onto which layers, scales, etc. can be added.
#' @importFrom ggplot2 ggplot ggplot_add aes_string scale_x_discrete scale_y_discrete
#' @rdname ggcor
#' @examples
#' df <- fortify_cor(mtcars)
#' ggcor(df)
#' df01 <- fortify_cor(mtcars, type = "lower", show.diag = FALSE)
#' ggcor(df01, axis.label.drop = TRUE)
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
ggcor <- function(data,
                  mapping = NULL,
                  axis.x.position = "auto",
                  axis.y.position = "auto",
                  drop = TRUE,
                  ...)
{
  if(!is_cor_tbl(data))
    stop("'data' needs a cor_tbl.", call. = FALSE)
  type <- get_type(data)
  show.diag <- get_show_diag(data)
  col.names <- get_col_name(data)
  row.names <- rev(get_row_name(data))

  base.aes <- aes_string(x = ".col.names", y = ".row.names")
  mapping <- if(is.null(mapping)) base.aes else aes_modify(base.aes, mapping)
  # handle axis setting
  axis.x.position <- match.arg(axis.x.position, c("auto", "bottom", "top"))
  axis.y.position <- match.arg(axis.y.position, c("auto", "left", "right"))
  if(axis.x.position == "auto") {
    axis.x.position <- switch (type,
                               full = "bottom",
                               lower = "bottom",
                               upper = "top")
  }
  if(axis.y.position == "auto") {
    axis.y.position <- switch (type,
                               full = "left",
                               lower = "left",
                               upper = "right")
  }

  if(isTRUE(drop)) {
    if(type != "full" && !isTRUE(show.diag)) {
      col.names <- if(type == "upper") {
        col.names[-1]
      } else {
        col.names[-length(col.names)]
      }
      row.names <- if(type == "upper") {
        row.names[-1]
      } else {
        row.names[-length(row.names)]
      }
    }
  }

  data$.col.names <- factor(data$.col.names, levels = col.names)
  data$.row.names <- factor(data$.row.names, levels = row.names)

  p <- ggplot(data = data, mapping = mapping, environment = parent.frame()) +
    scale_x_discrete(position = axis.x.position, drop = drop)+
    scale_y_discrete(position = axis.y.position, drop = drop)
  class(p) <- c("ggcor", class(p))
  p
}

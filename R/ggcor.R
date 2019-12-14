#' @export
ggcor <- function(data,
                  mapping = NULL,
                  axis.x.position = "auto",
                  axis.y.position = "auto",
                  axis.label.drop = TRUE,
                  ...)
{
  data <- fortify_cor(data, ...)
  type <- cor_tbl_type(data)
  show.diag <- cor_tbl_showdiag(data)
  xname <- cor_tbl_xname(data)
  yname <- cor_tbl_yname(data)
  base_map <- aes_string("x", "y")
  mapping <- if(is.null(mapping)) base_map else modifyList(base_map, mapping)
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
  axis.x.breaks <- 1:length(xname)
  axis.x.labels <- xname
  axis.y.breaks <- 1:length(yname)
  axis.y.labels <- yname
  if(axis.label.drop) {
    if(isFALSE(show.diag)) {
      if(type == "upper") {
        axis.x.breaks <- axis.x.breaks[-1]
        axis.x.labels <- axis.x.labels[-1]
        axis.y.breaks <- axis.y.breaks[-1]
        axis.y.labels <- axis.y.labels[-1]
      }
      if(type == "lower") {
        axis.x.breaks <- axis.x.breaks[-length(xname)]
        axis.x.labels <- axis.x.labels[-length(xname)]
        axis.y.breaks <- axis.y.breaks[-length(yname)]
        axis.y.labels <- axis.y.labels[-length(yname)]
      }
    }
  }
  envir <- parent.frame()
  p <- ggplot(data = data, mapping = mapping, environment = envir) +
    scale_x_continuous(breaks = axis.x.breaks, labels = axis.x.labels,
                       position = axis.x.position)+
    scale_y_continuous(breaks = axis.y.breaks, labels = axis.y.labels,
                       position = axis.y.position) +
    expand_limits(xlim = c(0.5, length(xname) + 0.5),
                  ylim = c(0.5, length(yname) + 0.5))
  class(p) <- c("ggcor", class(p))
  p
}

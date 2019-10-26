#' @export
theme_cor <- function(legend.position = "right",
                      ...)
{
  theme(
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.text.x.top = element_text(angle = 45, hjust = 0, vjust = 0),
    axis.text.x.bottom = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = NA),
    legend.position = legend.position,
    ...
  )
}

#' @export
remove_axis <- function(index = c("all", "x", "y")) {
  index <- match.arg(index)
  thm_mv_x <- ggplot2::theme(
    axis.title.x = element_blank(),
    axis.title.x.top = element_blank(),
    axis.title.x.bottom = element_blank(),
    axis.text.x = element_blank(),
    axis.text.x.top = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.line.x = element_blank(),
    axis.line.x.top = element_blank(),
    axis.line.x.bottom = element_blank()
  )
  thm_mv_y <- ggplot2::theme(
    axis.title.y = element_blank(),
    axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.y = element_blank(),
    axis.text.y.left = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.line.y = element_blank(),
    axis.line.y.left = element_blank(),
    axis.line.y.right = element_blank()
  )
  if(index == "all") {
    thm_mv_x + thm_mv_y
  } else if (index == "x"){
    thm_mv_x
  } else {
    thm_mv_y
  }
}



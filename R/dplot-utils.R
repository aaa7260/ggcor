#' Decoration plot
#' @title Decoration plot
#' @param .plot a "gg" or "dplot" object.
#' @param obj a "gg" object.
#' @param width,height scala numeric value.
#' @param pos one of "left", "right", "bottom" or "top".
#' @param colours colour palette for filling.
#' @param style style of plot, one of "corrplot" (default) or "ggplot".
#' @param title guide title.
#' @param breaks breaks of guide_colourbar.
#' @param labels labels of guide_colourbar.
#' @param limits limits of guide_colourbar.
#' @param nbin a numeric specifying the number of bins for drawing the guide_colourbar.
#' @return a "dplot" object.
#' @rdname dplot_utils
#' @importFrom ggplot2 ggplot geom_blank theme_viod ggplot_add
#' @importFrom patchwork plot_layout
#' @examples \dontrun{
#' library(ggplot2)
#' d <- matrix(sample(c(LETTERS[1:3], NA), 33, replace = TRUE), nrow = 11)
#' p <- gcor_tbl(d, "var") %>%
#'   quickcor(mapping = aes(colour = var)) +
#'   geom_point(size = 3)
#' quickcor(mtcars) +
#'   geom_square() +
#'   anno_row_custom(p)
#' }
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
as_dplot <- function(.plot) {
  if(is_dplot(.plot))
    return(.plot)
  if(!inherits(.plot, "gg")) {
    stop("'.plot' must be a 'gg' object.", call. = FALSE)
  }
  structure(.Data = .plot,
            .anno_info = list(row.anno = list(),
                         col.anno = list(),
                         width = NULL,
                         height = NULL,
                         r = 0,
                         l = 0,
                         t = 0,
                         b = 0),
            class = c("dplot", class(.plot)))
}

#' @rdname dplot_utils
#' @export
is_dplot <- function(.plot) {
  inherits(.plot, "dplot")
}

#' @rdname dplot_utils
#' @export
anno_col_custom <- function(obj,
                            height = 0.2,
                            pos = "top") {
  if(!inherits(obj, "gg")) {
    stop("'obj' must be a 'gg' object.", call. = FALSE)
  }
  structure(.Data = list(obj = obj, height = height, pos = pos),
            class = "anno_col_custom")
}

#' @rdname dplot_utils
#' @export
anno_row_custom <- function(obj,
                            width = 0.2,
                            pos = "right") {
  if(!inherits(obj, "gg")) {
    stop("'obj' must be a 'gg' object.", call. = FALSE)
  }
  structure(.Data = list(obj = obj, width = width, pos = pos),
            class = "anno_row_custom")
}

#' @noRd
.anno_row <- function(.plot,
                      obj,
                      width = 0.2,
                      pos = "right") {
  .plot <- as_dplot(.plot)
  if(!inherits(obj, "gg")) {
    stop("'obj' must be a 'gg' object.", call. = FALSE)
  }
  pos <- match.arg(pos, c("left", "right"))
  .anno_info <- attr(.plot, ".anno_info")
  if(pos == "left") {
    idx <- paste0(".left-", .anno_info$l + 1)
    .anno_info$l <- .anno_info$l + 1
    .anno_info$row.anno <- rlang::set_names(c(list(obj), .anno_info$row.anno),
                                       c(idx, names(.anno_info$row.anno)))
    .anno_info$width <- c(width, .anno_info$width)
  } else {
    idx <- paste0(".right-", .anno_info$r + 1)
    .anno_info$r <- .anno_info$r + 1
    .anno_info$row.anno <- rlang::set_names(c(.anno_info$row.anno, list(obj)),
                                       c(names(.anno_info$row.anno), idx))
    .anno_info$width <- c(.anno_info$width, width)
  }
  attr(.plot, ".anno_info") <- .anno_info
  .plot
}

#' @noRd
.anno_col <- function(.plot,
                      obj,
                      height = 0.2,
                      pos = "top") {
  .plot <- as_dplot(.plot)
  if(!inherits(obj, "gg")) {
    stop("'obj' must be a 'gg' object.", call. = FALSE)
  }
  pos <- match.arg(pos, c("top", "bottom"))
  .anno_info <- attr(.plot, ".anno_info")
  if(pos == "bottom") {
    .anno_info$b <- .anno_info$b + 1
    .anno_info$height <- c(height, .anno_info$height)
    .anno_info$col.anno <- c(list(obj), .anno_info$col.anno)
  } else {
    .anno_info$t <- .anno_info$t + 1
    .anno_info$height <- c(.anno_info$height, height)
    .anno_info$col.anno <- c(.anno_info$col.anno, list(obj))
  }
  attr(.plot, ".anno_info") <- .anno_info
  .plot
}

#' @export
ggplot_add.anno_row_custom <- function(object, plot, object_name) {
  .anno_row(plot, object$obj, object$width, object$pos)
}

#' @export
ggplot_add.anno_col_custom <- function(object, plot, object_name) {
  .anno_col(plot, object$obj, object$height, object$pos)
}

#' @rdname dplot_utils
#' @export
empty_plot <- function()
{
  ggplot() +
    geom_blank() +
    theme_void()
}

#' @noRd
liner_trans <- function (from, to)
{
  force(from)
  force(to)
  trans <- function(x) scales::rescale(x, from = from, to = to)
  inv <- function(x) scales::rescale(x, from = to, to = from)
  scales::trans_new(paste0("liner-from-", format(from), "-to-", format(to)),
                    transform = trans, inverse = inv)
}

#' @rdname dplot_utils
#' @export
print.dplot <- function(.plot,
                        colours = getOption("ggcor.fill.pal"),
                        style = getOption("ggcor.plot.style", "corrplot"),
                        title = "corr",
                        breaks = c(-1, -0.5, 0, 0.5, 1),
                        labels = c(-1, -0.5, 0, 0.5, 1),
                        limits = c(-1, 1),
                        nbin = 40) {

  .plot <- as_dplot(.plot)
  .anno_info <- attr(.plot, ".anno_info")
  row.anno <- .anno_info$row.anno
  col.anno <- .anno_info$col.anno
  r <- .anno_info$r
  l <- .anno_info$l
  t <- .anno_info$t
  b <- .anno_info$b
  width <- .anno_info$width
  height <- .anno_info$height

  if(inherits(.plot, "quickcor")) {
    style <- switch (style,
                     corrplot = "corrplot",
                     "ggplot2"
    )
    if(style == "corrplot") {
      mapping <- unclass(.plot$mapping)
      if(!is.null(mapping$fill) && is.null(.plot$scales$get_scales("fill"))) {
        fill.var.name <- as.character(rlang::quo_get_expr(mapping$fill))
        fill.var <- rlang::eval_tidy(mapping$fill, .plot$data)
        if(!is_general_cor_tbl(.plot$data) && fill.var.name == "r" &&
           is.numeric(fill.var)) {
          .plot <- .plot + scale_fill_gradient2n(colours = colours,
                                         breaks = breaks,
                                         labels = labels,
                                         limits = limits) +
            guides(fill = guide_colourbar(title = title,
                                          nbin  = nbin))
        }
      }
    }
  }
  class(.plot) <- setdiff(class(.plot), "dplot")
  if(length(row.anno) == 0 && length(col.anno) == 0) {
    grid::grid.draw(.plot)
  }

  n <- length(row.anno)
  m <- length(col.anno)

  plot.list <- vector("list", (m + 1) * (n + 1))
  plot.list <- lapply(seq_len((m + 1) * (n + 1)), function(.id) {
    plot.list[[.id]] <- empty_plot()
  })
  row.anno <- c(row.anno[seq_len(l)], list(.plot), row.anno[seq_len(r) + l])
  col.anno <- c(col.anno[seq_len(b)], list(.plot), col.anno[seq_len(t) + b])

  plot.list[t * (n + 1) + seq_len(n + 1)] <- row.anno
  plot.list[l + 1 + (n + 1) * (seq(m + 1) - 1)] <- rev(col.anno)
  width <- c(width[rev(seq_len(l))], 1, width[l + seq_len(r)])
  height <- c(height[rev(b + seq_len(t))], 1, height[rev(seq_len(b))])

  if(!.plot$coordinates$is_free()) {
    warning("'Height' and 'width' cannot be customized under",
    "a coordinates with fixed ratio.", call. = FALSE)
    width <- height <- NULL
  }
  p <- Reduce("+", plot.list) +
    plot_layout(ncol = n + 1,
                nrow = m + 1,
                byrow = TRUE,
                widths = width,
                heights = height,
                guides = "collect")
  grid::grid.draw(p)
}

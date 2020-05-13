#' Annotation for correlation matrix plot
#' @title Annotation for correlation matrix plot
#' @param bcols branch colours.
#' @param width,height width or height of tree.
#' @param pos position of tree.
#' @param colour,color colour of segments.
#' @param size width of segments.
#' @param linetype line type of segments.
#' @return anno_tree object.
#' @rdname anno_tree
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
anno_row_tree <- function(bcols = NULL,
                          width = NULL,
                          pos = NULL,
                          colour = NULL,
                          size = NULL,
                          linetype = NULL,
                          color
                          ) {
  if(!missing(color))
    colour <- color
  if(!is.null(pos)) {
    pos <- match.arg(pos, c("left", "right"))
  }
  structure(.Data = list(bcols = bcols, width = width, pos = pos,
                         colour = colour, size = size, linetype = linetype),
            class = "anno_row_tree")
}

#' @rdname anno_tree
#' @export
anno_col_tree <- function(bcols = NULL,
                          height = NULL,
                          pos = NULL,
                          colour = NULL,
                          size = NULL,
                          linetype = NULL,
                          color
) {
  if(!missing(color))
    colour <- color
  if(!is.null(pos)) {
    pos <- match.arg(pos, c("top", "bottom"))
  }
  structure(.Data = list(bcols = bcols, height = height, pos = pos,
                         colour = colour, size = size, linetype = linetype),
            class = "anno_col_tree")
}

#' Link annotation
#' @title Link annotation
#' layers of curves, nodes, and labels.
#' @param mapping aesthetic mappings parameters.
#' @param data a data frame.
#' @param width width of link plot.
#' @param pos position of link plot.
#' @param start.var,end.var character to specify which variable is the starting
#' points and which is the ending points. if the variable is not character, it
#' will be converted.
#' @param label.size,label.colour,label.family,label.fontface parameters for label.
#' @param nudge_x horizonal justification of label.
#' @param expand expansion of x axis.
#' @param ... extra parameters passing to layer function.
#' @return a ggplot layer.
#' @rdname anno_link
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
anno_link <- function(mapping = NULL,
                      data,
                      width = 0.3,
                      pos = "right",
                      label.size = 3.5,
                      label.colour = "black",
                      label.family = "",
                      label.fontface = 1,
                      nudge_x = 0.1,
                      expand = NULL,
                      start.var = NULL,
                      start.name = NULL,
                      end.var = NULL,
                      ...)
{
  start.var <- rlang::enquo(start.var)
  end.var <- rlang::enquo(end.var)
  structure(.Data = list(mapping = mapping, data = data, width = width, pos = pos,
                         label.size = label.size, label.colour = label.colour,
                         label.family = label.family, label.fontface = label.fontface,
                         nudge_x = nudge_x, start.var = start.var, start.name = start.name,
                         end.var = end.var, params = list(...)),
            class = "anno_link")
}

#' Square annotation
#' @title Square annotation
#' @description Draw the cluster square mark on the correlation matrix plot.
#' @param k an integer scalar or vector with the desired number of groups.
#' @param fill NA (default) or the fill colour of square.
#' @param colour,color the colour of square boder.
#' @param size size of square boder line.
#' @return square layer.
#' @rdname anno_hc_rect
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
anno_hc_rect <- function(k = 2,
                         fill = NA,
                         colour = "black",
                         size = 2,
                         color)
{
  if(!missing(color))
    colour <- color
  structure(.Data = list(k = k, fill = fill, colour = colour,
                         size = size), class = "anno_hc_rect")
}

#' Custom annotation
#' @title Custom annotation
#' @param mapping aesthetic mappings parameters.
#' @param data a data frame.
#' @param align align base on main plot.
#' @param pos position of annotation.
#' @param width,height width and height of annotation.
#' @param trans the name of a transformation object or the object itself.
#' @param remove.axis one of "auto", "x", "y" or "none".
#' @param theme theme object or the name.
#' @param ... extra parameters.
#' @return anno_* object.
#' @rdname anno_special
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
anno_bar <- function(mapping,
                     data,
                     align = TRUE,
                     pos = NULL,
                     width = 0.2,
                     height = 0.2,
                     trans = NULL,
                     remove.axis = "auto",
                     theme = theme_anno(),
                     ...) {
  remove.axis <- match.arg(remove.axis, c("auto", "x", "y", "all", "none"))
  structure(.Data = list(mapping = mapping, data = data, align = align,
                         pos = pos, width = width, height = height,
                         trans = trans, remove.axis = remove.axis,
                         theme = theme, params = list(...)),
            class = "anno_bar")
}

#' @rdname anno_special
#' @export
anno_bar2 <- function(mapping,
                      data,
                      align = TRUE,
                      pos = NULL,
                      width = 0.2,
                      height = 0.2,
                      trans = NULL,
                      remove.axis = "auto",
                      theme = theme_anno(),
                      ...) {
  remove.axis <- match.arg(remove.axis, c("auto", "x", "y", "all", "none"))
  structure(.Data = list(mapping = mapping, data = data, align = align,
                         pos = pos, width = width, height = height,
                         trans = trans, remove.axis = remove.axis,
                         theme = theme, params = list(...)),
            class = "anno_bar2")
}

#' @rdname anno_special
#' @export
anno_boxplot <- function(mapping,
                         data,
                         align = TRUE,
                         pos = NULL,
                         width = 0.2,
                         height = 0.2,
                         trans = NULL,
                         remove.axis = "auto",
                         theme = theme_anno(),
                         ...) {
  remove.axis <- match.arg(remove.axis, c("auto", "x", "y", "all", "none"))
  structure(.Data = list(mapping = mapping, data = data, align = align,
                         pos = pos, width = width, height = height,
                         trans = trans, remove.axis = remove.axis,
                         theme = theme, params = list(...)),
            class = "anno_boxplot")
}

#' @rdname anno_special
#' @export
anno_point <- function(mapping,
                       data,
                       align = TRUE,
                       pos = NULL,
                       width = 0.2,
                       height = 0.2,
                       remove.axis = "auto",
                       theme = theme_anno(),
                       ...) {
  remove.axis <- match.arg(remove.axis, c("auto", "x", "y", "all", "none"))
  structure(.Data = list(mapping = mapping, data = data, align = align,
                         pos = pos, width = width, height = height,
                         remove.axis = remove.axis, theme = theme,
                         params = list(...)),
            class = "anno_point")
}



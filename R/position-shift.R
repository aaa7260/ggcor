## These functions are inspired by the position_*() functions in the ggtreeExtra
## package and have been subtly modified for ggcor.

## Shuangbin Xu and Guangchuang Yu (2020). ggtreeExtra: An R Package To Add Geom
##   Layers On Circular Or Other Layout Tree Of "ggtree". R package version 0.0.0.9.
##   https://github.com/YuLab-SMU/ggtreeExtra/


#' Shift stack position function for annotation.
#' @title Pisition function with some shift and re-scale
#' @inheritParams ggplot2::position_stack
#' @param xshift,yshift the offsets on x-axis and y-axis.
#' @param xscale,yscale the range of the charts.
#' @return a position object.
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Position
#' @importFrom ggplot2 has_flipped_aes
#' @importFrom ggplot2 flip_data
#' @importFrom ggplot2 ggproto
#' @rdname position_shift_stack
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
position_shift_stack <- function(vjust = 1,
                                 xshift = NA,
                                 yshift = NA,
                                 xscale = NULL,
                                 yscale = NULL,
                                 reverse = FALSE) {
  ggproto(NULL,
          PositionShiftStack,
          vjust = vjust,
          xshift = xshift,
          yshift = yshift,
          xscale = xscale,
          yscale = yscale,
          reverse = reverse)
}

#' @export
#' @rdname position_shift_stack
position_shift_fill <- function(vjust = 1,
                                xshift = NA,
                                yshift = NA,
                                xscale = NULL,
                                yscale = NULL,
                                reverse = FALSE) {
  ggproto(NULL,
          PositionShiftFill,
          vjust = vjust,
          xshift = xshift,
          yshift = yshift,
          xscale = xscale,
          yscale = yscale,
          reverse = reverse)
}

#' @rdname position_shift_stack
#' @format NULL
#' @usage NULL
#' @export
PositionShiftStack <- ggproto(
  "PositionShiftStack", Position,
  type = NULL,
  vjust = 1,
  fill = FALSE,
  reverse = FALSE,
  xshift = NA,
  yshift = NA,
  xscale = NULL,
  yscale = NULL,

  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data)
    data <- flip_data(data, flipped_aes)
    list(
      var = self$var %||% stack_var(data),
      fill = self$fill,
      vjust = self$vjust,
      reverse = self$reverse,
      flipped_aes = flipped_aes,
      xshift = self$xshift,
      yshift = self$yshift,
      xscale = self$xscale,
      yscale = self$yscale
    )
  },

  setup_data = function(self, data, params) {
    data <- flip_data(data, params$flipped_aes)
    if(is.null(params$var)) {
      return(data)
    }

    data$ymax <- switch(params$var,
                        y = data$y,
                        ymax = ifelse(data$ymax == 0, data$ymin, data$ymax)
    )

    data <- remove_missing(
      data,
      vars = c("x", "xmin", "xmax", "y"),
      name = "position_shift_stack"
    )
    flip_data(data, params$flipped_aes)
  },

  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)
    if(is.null(params$var)) {
      return(data)
    }

    negative <- data$ymax < 0
    negative[is.na(negative)] <- FALSE

    neg <- data[negative, , drop = FALSE]
    pos <- data[!negative, , drop = FALSE]

    if(any(negative)) {
      neg <- collide(neg, NULL, "position_shift_stack", pos_stack,
                               vjust = params$vjust,
                               fill = params$fill,
                               reverse = params$reverse
      )
    }
    if(any(!negative)) {
      pos <- collide(pos, NULL, "position_shift_stack", pos_stack,
                               vjust = params$vjust,
                               fill = params$fill,
                               reverse = params$reverse
      )
    }

    data <- rbind(neg, pos)[match(seq_len(nrow(data)), c(which(negative), which(!negative))),]
    data <- flip_data(data, params$flipped_aes)

    if(!is.null(params$yscale)) {
      if(length(params$yscale[is.finite(params$yscale)]) != 1) {
        params$yscale <- NULL
      }
      if(!is.null(params$yscale) && params$yscale < 0) {
        params$yscale <- - params$yscale
      }
    }
    if(!is.null(params$xscale)) {
      if(length(params$xscale[is.finite(params$xscale)]) != 1) {
        params$xscale <- NULL
      }
      if(!is.null(params$xscale) && params$xscale < 0) {
        params$xscale <- - params$xscale
      }
    }
    if(!is.null(params$yscale)){
      yrange <- range(data$ymin, data$ymax, na.rm = TRUE)
      data$ymin <- scales::rescale(data$ymin, c(0, params$yscale), yrange)
      data$ymax <- scales::rescale(data$ymax, c(0, params$yscale), yrange)
    }
    if(!is.null(params$xscale)){
      xrange <- range(data$xmin, data$xmax, na.rm = TRUE)
      data$xmin <- scales::rescale(data$xmin, c(0, params$xscale), xrange)
      data$xmax <- scales::rescale(data$xmax, c(0, params$xscale), xrange)
    }

    if(!is.na(params$yshift)){
      data$ymin <- data$ymin + params$yshift
      data$ymax <- data$ymax + params$yshift
    }
    if(!is.na(params$xshift)){
      data$xmin <- data$xmin + params$xshift
      data$xmax <- data$xmax + params$xshift
    }

    data <- data.frame(data, check.names=FALSE)
  }
)

#' @rdname position_shift_stack
#' @format NULL
#' @usage NULL
#' @export
PositionShiftFill <- ggproto("PositionShiftFill",
                             PositionShiftStack,
                             fill = TRUE
)

#' Shift dodge position function for annotation.
#' @title Dodge function with some shift and re-scale
#' @inheritParams ggplot2::position_dodge
#' @param xshift,yshift the offsets on x-axis and y-axis.
#' @param xscale,yscale the range of the charts.
#' @return a position object.
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Position
#' @importFrom ggplot2 has_flipped_aes
#' @importFrom ggplot2 flip_data
#' @importFrom ggplot2 ggproto
#' @rdname position_shift_dodge
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
position_shift_dodge <- function(width = NULL,
                                 xshift = NA,
                                 yshift = NA,
                                 xscale = NULL,
                                 yscale = NULL,
                                 preserve = c("total", "single")) {
  ggproto(NULL,
          PositionShiftDodge,
          width = width,
          xshift = xshift,
          yshift = yshift,
          xscale = xscale,
          yscale = yscale,
          preserve = match.arg(preserve)
  )
}

#' @rdname position_shift_dodge
#' @format NULL
#' @usage NULL
#' @export
PositionShiftDodge <- ggproto(
  "PositionShiftDodge", Position,
  width = NULL,
  xshift = NA,
  yshift = NA,
  xscale = NULL,
  yscale = NULL,
  preserve = "total",
  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data)
    data <- flip_data(data, flipped_aes)
    if(is.null(data$xmin) && is.null(data$xmax) && is.null(self$width)) {
      warn("Width not defined. Set with `position_shift_dodge(width = ?)`")
    }

    if(identical(self$preserve, "total")) {
      n <- NULL
    } else {
      panels <- unname(split(data, data$PANEL))
      ns <- vapply(panels, function(panel) max(table(panel$xmin)), double(1))
      n <- max(ns)
    }

    list(
      width = self$width,
      xshift = self$xshift,
      yshift = self$yshift,
      xscale = self$xscale,
      yscale = self$yscale,
      n = n,
      flipped_aes = flipped_aes
    )
  },

  setup_data = function(self, data, params) {
    data <- flip_data(data, params$flipped_aes)
    if(!"x" %in% names(data) && all(c("xmin", "xmax") %in% names(data))) {
      data$x <- (data$xmin + data$xmax) / 2
    }
    flip_data(data, params$flipped_aes)
  },

  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)
    collided <- collide(
      data,
      params$width,
      name = "position_shift_dodge",
      strategy = pos_dodge,
      n = params$n,
      check.width = FALSE
    )
    data <- flip_data(collided, params$flipped_aes)
    data <- pos_shift_dodge(data = data,
                            xshift = params$xshift,
                            yshift = params$yshift,
                            xscale = params$xscale,
                            yscale = params$yscale)
  }
)



#' Shift dodge2 position function for annotation.
#' @title Dodge2 function with some shift and re-scale
#' @inheritParams ggplot2::position_dodge2
#' @param xshift,yshift the offsets on x-axis and y-axis.
#' @param xscale,yscale the range of the charts.
#' @return a position object.
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Position
#' @importFrom ggplot2 has_flipped_aes
#' @importFrom ggplot2 flip_data
#' @importFrom ggplot2 ggproto
#' @rdname position_shift_dodge2
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
position_shift_dodge2 <- function(width = NULL,
                                  preserve = c("total", "single"),
                                  xshift = NA,
                                  yshift = NA,
                                  xscale = NULL,
                                  yscale = NULL,
                                  padding = 0.1,
                                  reverse = FALSE) {
  ggproto(NULL,
          PositionShiftDodge2,
          width = width,
          preserve = match.arg(preserve),
          xshift = xshift,
          yshift = yshift,
          xscale = xscale,
          yscale = yscale,
          padding = padding,
          reverse = reverse
  )
}

#' @rdname position_shift_dodge2
#' @format NULL
#' @usage NULL
#' @export
PositionShiftDodge2 <- ggproto(
  "PositionShiftDodge2",
  PositionShiftDodge,
  preserve = "total",
  padding = 0.1,
  reverse = FALSE,

  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data)
    data <- flip_data(data, flipped_aes)
    if(is.null(data$xmin) && is.null(data$xmax) && is.null(self$width)) {
      warn("Width not defined. Set with `position_shift_dodge2(width = ?)`")
    }

    if(identical(self$preserve, "total")) {
      n <- NULL
    } else {
      panels <- unname(split(data, data$PANEL))
      if("x" %in% names(data)) {
        groups <- lapply(panels, function(panel) table(panel$x))
      } else {
        groups <- lapply(panels, find_x_overlaps)
      }
      n_groups <- vapply(groups, max, double(1))
      n <- max(n_groups)
    }

    list(
      width = self$width,
      n = n,
      xshift = self$xshift,
      yshift = self$yshift,
      xscale = self$xscale,
      yscale = self$yscale,
      padding = self$padding,
      reverse = self$reverse,
      flipped_aes = flipped_aes
    )
  },

  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)
    collided <- collide2(
      data,
      params$width,
      name = "position_shift_dodge2",
      strategy = pos_dodge2,
      n = params$n,
      padding = params$padding,
      check.width = FALSE,
      reverse = params$reverse
    )
    data <- flip_data(collided, params$flipped_aes)
    data <- pos_shift_dodge(data = data,
                            xshift = params$xshift,
                            yshift = params$yshift,
                            xscale = params$xscale,
                            yscale = params$yscale)
  }
)

#' Shift identity position function for annotation.
#' @title identity function with some shift and re-scale
#' @param xshift,yshift the offsets on x-axis and y-axis.
#' @param xscale,yscale the range of the charts.
#' @return a position object.
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Position
#' @importFrom ggplot2 has_flipped_aes
#' @importFrom ggplot2 flip_data
#' @importFrom ggplot2 ggproto
#' @rdname position_shift_identity
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
position_shift_identity <- function(xshift = NA,
                                    yshift = NA,
                                    xscale = NULL,
                                    yscale = NULL) {
  ggproto(NULL,
          PositionShiftIdentity,
          xshift = xshift,
          yshift = yshift,
          xscale = xscale,
          yscale = yscale)
}

#' @rdname position_shift_identity
#' @format NULL
#' @usage NULL
#' @export
PositionShiftIdentity <- ggproto(
  "PositionShiftIdentity",
  Position,
  xshift = NA,
  yshift = NA,
  xscale = NULL,
  yscale = NULL,
  setup_params = function(self, data){
    list(xshift = self$xshift,
         yshift = self$yshift,
         xscale = self$xscale,
         yscale = self$yscale)
  },

  compute_layer = function(self, data, params, layout) {
    data <- pos_shift_identity(data,
                               xshift = params$xshift,
                               yshift = params$yshift,
                               xscale = params$xscale,
                               yscale = params$yscale)

    data
  }
)

#' @noRd
pos_shift_dodge <- function(data, xshift, yshift, xscale, yscale){
  name <- names(data)
  ypos <- intersect(c("ymin", "ymax", "lower", "middle", "upper", "y", "notchupper",
                      "notchlower", "outliers", "ymin_final", "ymax_final"), name)
  xpos <- intersect(c("xmin", "xmax", "xlower", "xmiddle", "xupper", "notchupper",
                      "notchlower", "outliers", "xmin_final", "xmax_final"), name)

  if(!is.null(yscale)){
    yrange <- range(unlist(data[ypos]), na.rm = TRUE)
    data <- purrr::map2(data, name, function(y, name) {
      if(!name %in% ypos) {
        return(y)
      }
      if(name == "outliers") {
        purrr::map(y, function(.y) {
          if(length(.y) == 0) {
            return(.y)
          }
          scales::rescale(.y, c(0, yscale), yrange)
        })
      } else {
        scales::rescale(y, c(0, yscale), yrange)
      }
    })
  }

  if(!is.null(xscale)){
    xrange <- range(unlist(data[xpos]), na.rm = TRUE)
    data <- purrr::map2(data, name, function(x, name) {
      if(!name %in% xpos) {
        return(x)
      }
      if(name == "outliers") {
        purrr::map(x, function(.x) {
          if(length(.x) == 0) {
            return(.x)
          }
          scales::rescale(.x, c(0, xscale), xrange)
        })
      } else {
        scales::rescale(x, c(0, xscale), xrange)
      }
    })
  }

  if(!is.na(yshift)){
    data <- purrr::map2(data, name, function(y, name) {
      if(!name %in% ypos) {
        return(y)
      }
      if(name == "outliers") {
        purrr::map(y, function(.y) {
          if(length(.y) == 0) {
            return(.y)
          }
          .y + yshift
        })
      } else {
        y + yshift
      }
    })
  }

  if(!is.na(xshift)){
    data <- purrr::map2(data, name, function(x, name) {
      if(!name %in% xpos) {
        return(x)
      }
      if(name == "outliers") {
        purrr::map(x, function(.x) {
          if(length(.x) == 0) {
            return(.x)
          }
          .x + xshift
        })
      } else {
        x + xshift
      }
    })
  }
  data <- new_data_frame(data)
}

#' @noRd
pos_shift_identity <- function(data, xshift, yshift, xscale, yscale){
  name <- names(data)
  ypos <- intersect(c("ymin", "ymax", "y", "yend"), name)
  xpos <- intersect(c("xmin", "xmax", "x", "xend"), name)

  if(!is.null(yscale)){
    yrange <- range(unlist(data[ypos]), na.rm = TRUE)
    data <- purrr::map2(data, name, function(y, name) {
      if(!name %in% ypos) {
        return(y)
      }
      scales::rescale(y, c(0, yscale), yrange)
    })
  }

  if(!is.null(xscale)){
    xrange <- range(unlist(data[xpos]), na.rm = TRUE)
    data <- purrr::map2(data, name, function(x, name) {
      if(!name %in% xpos) {
        return(x)
      }
      scales::rescale(x, c(0, xscale), xrange)
    })
  }

  if(!is.na(yshift)){
    data <- purrr::map2(data, name, function(y, name) {
      if(!name %in% ypos) {
        return(y)
      }
      y + yshift
    })
  }

  if(!is.na(xshift)){
    data <- purrr::map2(data, name, function(x, name) {
      if(!name %in% xpos) {
        return(x)
      }
      x + xshift
    })
  }
  data <- new_data_frame(data)
}

#' @noRd
collide <- getFromNamespace("collide", "ggplot2")

#' @noRd
collide2 <- getFromNamespace("collide2", "ggplot2")

#' @noRd
stack_var <- getFromNamespace("stack_var", "ggplot2")

#' @noRd
pos_stack <- getFromNamespace("pos_stack", "ggplot2")

#' @noRd
pos_dodge <- getFromNamespace("pos_dodge", "ggplot2")

#' @noRd
pos_dodge2 <- getFromNamespace("pos_dodge2", "ggplot2")

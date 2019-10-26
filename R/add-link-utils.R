#' @noRd
#' @importFrom dplyr %>%
handle_link_data <- function(
  x,
  y,
  corrmat.point.hjust = NULL,
  corrmat.point.vjust = NULL,
  group.point.hjust = NULL,
  group.point.vjust = NULL,
  on.left = FALSE,
  diag.label = FALSE)
{
  if(!inherits(y, "cor_tbl"))
    stop("'y' need a cor_tbl.", call. = FALSE)
  xname <- cor_tbl_xname(y)
  yname <- cor_tbl_yname(y)
  type <- cor_tbl_type(y)
  show.diag <- cor_tbl_showdiag(y)
  group.name <- unique(x$x)
  y <- cor_tbl_namebind(y)
  corrmat_data <- link_corrmat_data(yname = yname,
                                    n.row = length(yname),
                                    n.col = length(xname),
                                    type = type,
                                    show.diag = show.diag,
                                    hjust = corrmat.point.hjust,
                                    vjust = corrmat.point.vjust,
                                    on.left = on.left,
                                    diag.label = diag.label)
  group_data <- link_group_data(group.name = group.name,
                                n.row = length(yname),
                                n.col = length(xname),
                                type = type,
                                hjust = group.point.hjust,
                                vjust = group.point.vjust,
                                on.left = on.left)
  link_data <- x %>%
    dplyr::left_join(corrmat_data, by = c("y" = "corr_yname")) %>%
    dplyr::left_join(group_data, by = c("x" = "group_name"))
  structure(list(link_data = link_data,
                 corrmat_data = corrmat_data,
                 group_data = group_data),
            class = "mantel_link_data")
}

#' @noRd
link_group_data <- function(group.name, n.row, n.col, type, hjust = NULL, vjust = NULL, on.left = FALSE)
{
  len <- length(group.name)
  if(!is.null(hjust) && length(hjust) != len)
    hjust <- rep_len(hjust, len)
  if(!is.null(vjust) && length(hjust) != len)
    vjust <- rep_len(vjust, len)
  if(type == "full") {
    y <- seq(0.5, n.row + 0.5, length.out = len + 2)[2:(len + 1)]
    if(on.left) {
      x <- rep_len(0.5 - 0.40 * n.col, len)
    } else {
      x <- rep_len(n.col + 0.5 + 0.40 * n.col, len)
    }
  } else if(type == "upper") {
    if(len == 1) {
      x <- 0.5 + 0.18 * n.col
      y <- 0.5 + 0.3 * n.row
    } else if(len == 2) {
      x <- c(0.5 - 0.02 * n.col, 0.5 + 0.2 * n.col)
      y <- c(0.5 + 0.46 * n.row, 0.5 + 0.2 * n.row)
    } else {
      y0 <- 0.5 + n.row * (1 - 0.3)
      y1 <- 0.5 + n.row * 0.1
      x0 <- 0.5 - 0.25 * n.col
      x1 <- 0.5 + 0.3 * n.col
      y <- seq(y0, y1, length.out = len)
      x <- seq(x0, x1, length.out = len)
    }
  } else {
    if(len == 1) {
      x <- 0.5 + 0.82 * n.col
      y <- 0.5 + 0.7 * n.row
    } else if(len == 2) {
      x <- c(0.5 + 0.8 * n.col, 0.5 + 1.02 * n.col)
      y <- c(0.5 + 0.8 * n.row, 0.5 + 0.54 * n.row)
    } else {
      y0 <- 0.5 + n.row * (1 - 0.1)
      y1 <- 0.5 + n.row * 0.3
      x0 <- 0.5 + 0.75 * n.col
      x1 <- 0.5 + 1.3 * n.col
      y <- seq(y0, y1, length.out = len)
      x <- seq(x0, x1, length.out = len)
    }
  }
  if(!is.null(hjust))
    x <- x + hjust
  if(!is.null(vjust))
    y <- y + vjust
  data.frame(group_x = x,
             group_y = y,
             group_name = group.name,
             stringsAsFactors = FALSE)
}

#' @noRd
link_corrmat_data <- function(yname, n.row, n.col, type = "upper", show.diag = FALSE,
                              hjust = NULL, vjust = NULL, on.left = FALSE, diag.label = FALSE)
{
  if(!is.null(hjust) && length(hjust) != n.row)
    hjust <- rep_len(hjust, n.row)
  if(!is.null(vjust) && length(hjust) != n.row)
    vjust <- rep_len(vjust, n.row)
  x <- n.col:1
  y <- 1:n.row
  if(type == "full") {
    if(on.left) {
      x <- rep_len(0, n.row)
    } else {
      x <- rep_len(n.col + 1, n.row)
    }
  } else {
    if(type == "upper") {
      if(show.diag) {
        if(diag.label) {
          x <- x - 2
        } else {
          x <- x - 1
        }
      } else {
        if(diag.label) {
          x <- x - 1
        }
      }
    } else {
      if(show.diag) {
        if(diag.label) {
          x <- x + 2
        } else {
          x <- x + 1
        }
      } else {
        if(diag.label) {
          x <- x + 1
        }
      }
    }
  }
  if(!is.null(hjust))
    x <- x + hjust
  if(!is.null(vjust))
    y <- y + vjust
  data.frame(corr_x = x,
             corr_y = y,
             corr_yname = yname,
             stringsAsFactors = FALSE)
}


#' @noRd
rename_mantel <- function(mantel) {
  name <- names(mantel)
  name <- ifelse(name == "spec", "x", name)
  name <- ifelse(name == "env", "y", name)
  names(mantel) <- name
  mantel
}


#' @noRd
link_colour_pal <- function(n)
{
  stopifnot(n <= 7)
  colors <- c("#7A0177", "#D95F02", "#1B9E77", "#7570B3",
              "#E7298A", "#A6761D", "#CCCCCC")
  if(n == 1)
    return(colors[1])
  col <- c(colors[1:(n - 1)], colors[7])
  col

}

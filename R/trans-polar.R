trans_polar <- function(data, 
                        start.angle = NULL,
                        end.angle = NULL,
                        inner.radius = 0.4,
                        outer.radius = 1,
                        split.by.group = FALSE,
                        group.space = 1, ## in degree
                        ...) 
{
  if(!is_cor_tbl(data)) {
    data <- fortify_cor(data, ...)
  }
  is.symmet <- is_symmet(data)
  row.names <- get_row_name(data)
  col.names <- get_col_name(data)
  grouped <- attr(data, "grouped")
  type <- get_type(data)
  n <- length(row.names)
  m <- length(col.names)
  
  if(is.null(start.angle)) {
    start.angle <- if(is.symmet) 0 else 1 / 12 * pi
  } else {
    start.angle <- degree_radius(start.angle %% 360)
  }
  if(is.null(end.angle)) {
    end.angle <- if(is.symmet) 2 * pi else 23 / 12 * pi
  } else {
    end.angle <- degree_radius(end.angle %% 360)
  }
  
  if(start.angle > end.angle) {
    temp <- start.angle
    start.angle <- end.angle
    end.angle <- temp
  }
  
  if(inner.radius > outer.radius) {
    temp <- inner.radius
    inner.radius <- outer.radius
    outer.radius <- temp
  }
  
  t <- seq(start.angle, end.angle, length.out = m + 1)
  r <- seq(inner.radius, outer.radius, length.out = n + 1)
  
  if(isTRUE(grouped) && isTRUE(split.by.group)) {
    data <- split(data, data$.group)
    group.space <- degree_radius(group.space)
    enclose <- identical(start.angle, end.angle - 2 * pi) || 
               ((end.angle -  start.angle) < group.space)
    l <- length(data)
    cell.angle <- if(enclose) {
      (end.angle - start.angle) / l - group.space
    } else {
      (end.angle - start.angle -  group.space * (l - 1)) / l
    }

    data <- purrr::map_dfr(seq_along(data), function(.id) {
      angle1 <- start.angle + (.id - 1) * (cell.angle + group.space)
      angle2 <- angle1 + cell.angle
      t <- seq(angle1, angle2, length.out = m + 1)
      r <- seq(inner.radius, outer.radius, length.out = n + 1)
      pos <- tibble::tibble(start = t[-(m + 1)][data[[.id]]$.col.id],
                            end = t[-1][data[[.id]]$.col.id],
                            r1 = r[-(n + 1)][data[[.id]]$.row.id],
                            r2 = r[-1][data[[.id]]$.row.id])
      dplyr::bind_cols(pos, data[[.id]])
    })
  } else {
    pos <- tibble::tibble(start = t[-(m + 1)][data$.col.id],
                          end = t[-1][data$.col.id],
                          r1 = r[-(n + 1)][data$.row.id],
                          r2 = r[-1][data$.row.id])
    data <- dplyr::bind_cols(pos, data)
  }
  
  mid.angle <- ((start.angle + end.angle) / 2) + pi
  
  axis.x.pos <- tibble::tibble(x = 1.05 * sin((t[-(m + 1)] + t[-1]) / 2),
                               y = 1.05 * cos((t[-(m + 1)] + t[-1]) / 2),
                               label = col.names,
                               angle = ggraph::node_angle(x, y))
  
  mid.r <- (r[-(n + 1)] + r[-1]) / 2
  axis.y.pos <- tibble::tibble(x = mid.r * sin(mid.angle),
                               y = mid.r * cos(mid.angle),
                               label = rev(row.names),
                               angle = radius_degree(mid.angle) %% 360)

  structure(.Data = data,
            axis.x.pos = axis.x.pos,
            axis.y.pos = axis.y.pos,
            type = type,
            grouped = grouped,
            class = c("polar_tbl", class(data))
            )
}

degree_radius <- function(x) {
  x / 180 * pi
}

radius_degree <- function(x) {
  x * 180 / pi
}

trans_polar(correlate(mtcars)) %>% 
  ggplot() +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = r0, r = r, 
                   start = start, end = end, fill = r),
               colour = "grey90", size = 0.25) +
  coord_fixed() +
  theme_void()

data(varespec, package = "vegan")
data(varechem, package = "vegan")
xx <- fortify_cor(varechem, varespec, 
                  group = paste0("Group", rep_len(1:2, 24))) %>% 
  trans_polar(group.space = 90, split.by.group = T) 
ggplot(xx) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = r1, r = r2, 
                   start = start, end = end, fill = r),
               colour = "grey90", size = 0.25) +
  scale_fill_gradient2n() +
  coord_fixed(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) +
  theme_void()

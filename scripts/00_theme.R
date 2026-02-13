
library(ggplot2)
library(grid)


export_width  <- 10   
export_height <- 3  
export_dpi    <- 300

update_geom_defaults("line", list(size = 0.5, colour = "black"))

ba_theme <- function(legend_pos = c("none", "right"), base_size = 14, base_family = "sans") {
  legend_pos <- match.arg(legend_pos)
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title       = ggplot2::element_blank(),
      plot.subtitle    = ggplot2::element_blank(),
      axis.title       = ggplot2::element_text(face = "bold", size = 16),
      axis.title.x     = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y     = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      axis.text        = ggplot2::element_text(size = 12),
      axis.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.text.y.left   = ggplot2::element_text(margin = ggplot2::margin(r = 6)),
      axis.ticks.length  = grid::unit(0.25, "cm"),
      axis.line  = ggplot2::element_line(color = "black", size = 0.4),
      legend.position  = legend_pos,
      legend.title     = ggplot2::element_blank(),
      legend.text      = ggplot2::element_text(size = 12),
      panel.grid.major.y = ggplot2::element_line(color = "gray70", size = 0.5, linetype = "dashed"),
      panel.grid.minor.y = ggplot2::element_line(color = "gray85", size = 0.4, linetype = "dashed"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10,10,10,10)
    )
}


# reference grid lines --------------------------------------------------------------

# we have to manually create grid lines because we want empty space before t=1

geom_gridline <- function(index_y = NULL, baseline = FALSE) {
  
  if (baseline) {
    
    line_y <- 100
    line_linetype <- "solid"
    line_linewidth <- 0.9
    
    # label on y-axis
    ann_size <- 5
    ann_lab <- plot_baseline_lab
    ann_fontface <- 3
    
  } else {
    
    line_y <- plot_ybreaks[index_y]
    line_linetype <- "dotted"
    line_linewidth <- 0.7
    
    # label on y-axis
    ann_size <- 6
    ann_lab <- plot_ybreaks_lab[index_y]
    ann_fontface <- 1
    
  }
  
  line_end <- 2022
  
  # provide both the grid line and its label
  list(
    geom_segment(x = plot_xmin, xend = line_end, col = palette_plot_elem, # constant 
                 y = line_y, yend = line_y, 
                 linetype = line_linetype, linewidth = line_linewidth),
    
    annotate("text", x = plot_gridline_x, # constant
             colour = palette_plot_elem, family = plot_fontfamily, # constant
             y = line_y, label = ann_lab, size = ann_size, fontface = ann_fontface)
  )
  
}


# x-axis brackets -------------------------------------------------------------------

geom_axisbracket <- function(bracket_type = "time", bracket_trend = cur_trend) {
  
  if (bracket_type == "time") {
    
    bracket_min <- timegroups_bracket_min
    bracket_max <- timegroups_bracket_max
    bracket_ypos <- plot_ymin0 - 0.01 * plot_range_max
    bracket_lab <- timegroups_lab[-1]
    bracket_tiplength <- 0
    bracket_labelsize <- 4.8
    bracket_fontface <- 1
    bracket_vjust <- 2.5
    
  } else if (bracket_type == "trend") {
    
    if (bracket_trend == "CAT") {
      return("Current Trend bracket should not be plotted in the CAT plot!")
    }
    
    bracket_min <- 2015 - 0.5
    bracket_max <- 2022 + 0.5
    bracket_ypos <- plot_ymin0 - 0.065 * plot_range_max
    bracket_lab <- "Current Trend"
    bracket_tiplength <- -0.017
    bracket_labelsize <- 5
    bracket_fontface <- 3
    bracket_vjust <- 2.7
    
  }
  
  bracket_shorten <- 0.15
  
  geom_bracket(inherit.aes = FALSE, family = plot_fontfamily, col = palette_plot_elem, # constant
               xmin = bracket_min, xmax = bracket_max, y.position = bracket_ypos,
               label = bracket_lab, label.size = bracket_labelsize, fontface = bracket_fontface,
               tip.length = bracket_tiplength, vjust = bracket_vjust, bracket.shorten = bracket_shorten)
  
}


# SoIB trend plot theme -------------------------------------------------------------

ggtheme_soibtrend <- function() {
  
  theme_void() +
    theme(axis.title.y = element_text(size = plot_ylab_size, colour = palette_plot_elem,
                                      angle = 90, margin = plot_ytitle_margin),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          plot.title = element_text(face = "bold", size = 20, hjust = 0.5, vjust = -2, 
                                    colour = palette_plot_title),
          text = element_text(family = plot_fontfamily),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))
  
}

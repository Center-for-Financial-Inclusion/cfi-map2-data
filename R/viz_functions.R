######################################################
# Visualization functions -----------------------
######################################################

fig_bar <- function(data, xvar, yvar, facets = NULL, labels, barparams, palette = NULL, coord_flip = FALSE, yaxtype = "number", legend = TRUE) {

  xvar <- sym(xvar)
  yvar <- sym(yvar)

  p <- ggplot(
    data = data,
    aes(
      x = !!xvar,
      y = !!yvar
    )
  ) +

    # Geoms
    geom_col(fill =  barparams[["bars"]][["color"]], width =  barparams[["bars"]][["width"]])

    # Error bars
    if (barparams[["errorbars"]][["show"]]) {

      ymin <- sym(names(data)[str_detect(names(data), "_upp")])
      ymax <- sym(names(data)[str_detect(names(data), "_low")])
      p <- p + geom_linerange(aes(ymin = !!ymin, ymax = !!ymax), color = barparams[["errorbars"]][["color"]])

    }

  if (barparams[["valuelabels"]][["show"]]) {

    maxy <- max(data[[yvar]], na.rm = TRUE)
    ndg_y <- maxy/barparams[["valuelabels"]][["lab_ndgfcty"]]
    ndg_y <- ndg_y*barparams[["valuelabels"]][["lab_ndgdiry"]]
    ndg_x <- barparams[["valuelabels"]][["lab_ndgx"]]

    p <- p +
    # Valuelabel
    geom_text(aes(label = valuelabel), hjust = barparams[["valuelabels"]][["lab_hjust"]], nudge_y = ndg_y, nudge_x = ndg_x, fontface = barparams[["valuelabels"]][["lab_face"]])

  }

    p <- p +
    # Legend
    guides(fill = guide_legend(title = labels[["legend_ti"]], nrow = 1)) +

    # Figure Labels
    labs(
      title = labels[["title"]],
      subtitle = labels[["subtitle"]],
      y = labels[["yax_ti"]],
      x = labels[["xax_ti"]],
      caption = labels[["caption"]]
    ) +

    theme_custom() +
    theme(legend.position = "top")

  if (!is.null(facets)) {
    fcvar_rows <- sym(facets[["rows"]])
    fcvar_cols <- sym(facets[["cols"]])

    if (facets[["type"]] == "wrap") {

      p <- p + facet_wrap(vars(str_wrap(!!fcvar_cols, 20)), nrow = 1, scales = facets[["scales"]])

    } else if (facets[["type"]] == "grid") {

      p <- p + facet_grid(rows = vars(str_wrap(!!fcvar_rows, 20)), cols = vars(str_wrap(!!fcvar_cols, 20)), scales = facets[["scales"]], switch = "y", space = facets[["space"]])

      }

  }

  # Scales
  if (!is.null(palette)) {
    p <- p + scale_fill_manual(values = palette)
  }

  if (coord_flip) {
    p <- p + coord_flip()
  }

  if (yaxtype == "number") {
    p <- p +  scale_y_continuous(position = "right", labels = scales::label_comma(), expand = c(0,0))
  }
  else if (yaxtype == "percent") {
    breaks <- c(0, .20, .40, .60, .80, 1)
    p <- p +  scale_y_continuous(position = "right", breaks = breaks, labels = scales::label_percent(suffix = ""), expand = c(0,0)) + 
      geom_hline(yintercept = breaks, color ="white")
  }

  if (legend == FALSE) {
    p <- p + theme(legend.position = "none")
  }

  return(p)

}

fig_bar_w_fill <- function(data, xvar, yvar, fvar, facets, labels, barparams, palette = NULL,  coord_flip = FALSE, yaxtype = "number", yaxdroplines = FALSE, legend = TRUE, errorbars = FALSE) {

xvar <- sym(xvar)
yvar <- sym(yvar)
fvar <- sym(fvar)

p <- ggplot(
  data = data,
  aes(
    x = !!xvar,
    y = !!yvar,
    fill = !!fvar
  )
)

  # Geoms
  if (barparams[["bars"]][["position"]] == "stack") {
    pos <- position_stack(vjust = barparams[["valuelabels"]][["lab_vjust"]])
  } else if (barparams[["bars"]][["position"]] == "dodge") {
    pos <- position_dodge(width = barparams[["bars"]][["position_width"]])
  }

  p <- p + geom_col(width =  barparams[["bars"]][["width"]], color = barparams[["bars"]][["color"]], position = pos)

  # Value labels
  if (barparams[["valuelabels"]][["show"]]) {
    p <- p +
      geom_text(aes(label = valuelabel), position = pos, size = barparams[["valuelabels"]][["lab_size"]], hjust = barparams[["valuelabels"]][["lab_hjust"]], fontface = barparams[["valuelabels"]][["lab_face"]])
  }

  # Error bars
  if (barparams[["errorbars"]][["show"]]) {

    ymin <- sym(names(data)[str_detect(names(data), "_upp")])
    ymax <- sym(names(data)[str_detect(names(data), "_low")])
    p <- p + geom_linerange(aes(ymin = !!ymin, ymax = !!ymax), position = pos, color = barparams[["errorbars"]][["color"]])

  }

  if (barparams[["catlabels"]][["show"]]) {
    p <- p + geom_text(aes(y = 0, label = catlabel), position = pos, hjust = 0, size = 2.5, fontface = "bold")
  }

  # Legend
  p <-  p + guides(fill = guide_legend(title = labels[["legend_ti"]], direction = labels[["legend_dir"]], nrow = labels[["legend_nrow"]], reverse = labels[["legend_rev"]])) +

  # Figure Labels
  labs(
    title = labels[["title"]],
    subtitle = labels[["subtitle"]],
    y = labels[["yax_ti"]],
    x = labels[["xax_ti"]],
    caption = labels[["caption"]]
  ) +
  
  theme_custom() 

  if (!is.null(facets)) {
    fcvar_rows <- sym(facets[["rows"]])
    fcvar_cols <- sym(facets[["cols"]])

    if (facets[["type"]] == "wrap") {

      p <- p + facet_wrap(vars(str_wrap(!!fcvar_cols, 25)), nrow = 1, scales = facets[["scales"]])

    } else if (facets[["type"]] == "grid") {

      p <- p + facet_grid(rows = vars(str_wrap(!!fcvar_rows, 25)), cols = vars(str_wrap(!!fcvar_cols, 25)), scales = facets[["scales"]], switch = "y", space = facets[["space"]])

    }

  }

# Scales
if (!is.null(palette)) {
    p <- p + scale_fill_manual(values = palette)
}

if (coord_flip) {
  p <- p + coord_flip()
}

if (barparams[["bars"]][["labeltotal"]]) {
  maxy <- max(data[["barlabelpos"]], na.rm = TRUE)
  ndg_y <- maxy/60
  p <- p +
    geom_text(aes(y = barlabelpos, label = barlabel), hjust = 0, nudge_y = ndg_y, fontface = "bold") 
}

if (yaxtype == "number") {
  if (barparams[["bars"]][["labeltotal"]]) {
    p <- p +  scale_y_continuous(position = "right", limits = c(0, maxy + 0.15*maxy), labels = scales::label_comma(), expand = c(0,0))
  } else { 
    p <- p +  scale_y_continuous(position = "right", labels = scales::label_comma(), expand = c(0,0))
    }
}
  
if (yaxtype == "percent") {
    breaks <- c(0, .20, .40, .60, .80, 1)
    p <- p +  scale_y_continuous(position = "right", breaks = breaks, labels = scales::label_percent(suffix = "")) 
}
 
if (yaxdroplines == TRUE) { 
  p <- p + geom_hline(yintercept = breaks, color ="white")
} 
  
if (legend == FALSE) {
  p <- p + theme(legend.position = "none")
}

return(p)

}

fig_tile <- function(data, xvar, yvar, fvar, facets, labels, tileparams, palette = NULL,  coord_flip = FALSE, yaxtype = "number", legend = TRUE, errorbars = FALSE) {
  
  xvar <- sym(xvar)
  yvar <- sym(yvar)
  fvar <- sym(fvar)
  
  p <- ggplot(
    data = data,
    aes(
      x = !!xvar,
      y = !!yvar,
      fill = !!fvar
    )
  )
  
  # Geoms

  p <- p + geom_tile(color = tileparams[["tiles"]][["color"]])
  
  # Value labels
  if (tileparams[["valuelabels"]][["show"]]) {
    p <- p +
      geom_text(aes(label = valuelabel), size = tileparams[["valuelabels"]][["lab_size"]], hjust = tileparams[["valuelabels"]][["lab_hjust"]], fontface = tileparams[["valuelabels"]][["lab_face"]])
  }
  
  # Legend & scales
  p <-  p + guides(fill = guide_legend(title = labels[["legend_ti"]], nrow = 1)) + scale_y_discrete(position = "right") +
    
    # Figure Labels
    labs(
      title = labels[["title"]],
      subtitle = labels[["subtitle"]],
      y = labels[["yax_ti"]],
      x = labels[["xax_ti"]],
      caption = labels[["caption"]]
    ) +
    
    theme_custom() 
  
  if (!is.null(facets)) {
    fcvar_rows <- sym(facets[["rows"]])
    fcvar_cols <- sym(facets[["cols"]])
    
    if (facets[["type"]] == "wrap") {
      
      p <- p + facet_wrap(vars(str_wrap(!!fcvar_cols, 30)), nrow = 1, scales = facets[["scales"]])
      
    } else if (facets[["type"]] == "grid") {
      
      p <- p + facet_grid(rows = vars(!!fcvar_rows), cols = vars(!!fcvar_cols), scales = facets[["scales"]], switch = "y", space = facets[["space"]])
      
    }
    
  }
  
  # Scales
  if (!is.null(palette)) {
    if(palette == "identity") { 
      p <- p + scale_fill_identity()
    }
    else {
      p <- p + scale_fill_distiller(palette = palette, direction = 1)
    }
  }
  
  if (coord_flip) {
    p <- p + coord_flip()
  }
  
  if (legend == FALSE) {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
  
}


fig_regests <- function(data, labels, facets, barparams, scales, coord_flip = FALSE) {
  
  data <- data %>% group_by(model_type) %>% 
    mutate(x = ifelse(term == "(Intercept)", fig_data, NA), 
           refline = max(x, na.rm = TRUE), 
           refline = ifelse(row_number() == 1, refline, NA))
  
  p <- ggplot(data = data,
         aes(x = fct_rev(fct_inorder(str_wrap(effect_label, scales[["x"]][["txtwrap"]]))),
             y = fig_data))
    
    if (!is.null(facets)) {
      fcvar_rows <- sym(facets[["rows"]])
      fcvar_cols <- sym(facets[["cols"]])
      
      if (facets[["type"]] == "wrap") {
        
        p <- p + facet_wrap(vars(str_wrap(!!fcvar_cols, 30)), nrow = 1, scales = facets[["scales"]])
        
      } else if (facets[["type"]] == "grid") {
        
        p <- p + facet_grid(rows = vars(!!fcvar_rows), cols = vars(!!fcvar_cols), scales = facets[["scales"]], switch = "y", space = facets[["space"]])
        
      }
      
    }
  
    p + geom_col(width =  barparams[["bars"]][["width"]], fill = barparams[["bars"]][["color"]]) + 
    geom_hline(aes(yintercept = refline), color = "red", size = 0.25, linetype = "dashed") + 
    geom_segment(aes(y = startarrow, yend = endarrow), color = "red", arrow = arrow(length=unit(.2, 'cm'), type = "closed"), size = 1.5) +
    geom_point(aes(y = startarrow), shape = 21, color = "red", fill = "white", size= 3.5) -> p
    
    # Value labels
    if (barparams[["valuelabels"]][["show"]]) {
        p <- p +
          geom_text(aes(label = barlabel), color = "black", 
                    size = barparams[["valuelabels"]][["lab_size"]], 
                    nudge_x = barparams[["valuelabels"]][["lab_ndgx"]], 
                    nudge_y = barparams[["valuelabels"]][["lab_ndgy"]], 
                    hjust = barparams[["valuelabels"]][["lab_vjust"]], 
                    hjust = barparams[["valuelabels"]][["lab_hjust"]], 
                    fontface = barparams[["valuelabels"]][["lab_face"]])
    }
    # Arrow labels
      if (barparams[["arrowlabels"]][["show"]]) {
        p <- p +
          geom_text(aes(y = valuelabel_pos, label = valuelabel), color = "red", 
                    size = barparams[["arrowlabels"]][["lab_size"]], 
                    nudge_x = barparams[["arrowlabels"]][["lab_ndgx"]], 
                    nudge_y = barparams[["arrowlabels"]][["lab_ndgy"]], 
                    hjust = barparams[["arrowlabels"]][["lab_vjust"]], 
                    hjust = barparams[["arrowlabels"]][["lab_hjust"]], 
                    fontface = barparams[["arrowlabels"]][["lab_face"]])
      }  
      
    #geom_text(aes(y = 0.95, label = annotation), hjust = 0, color = "black", nudge_x = 0, size = 3.5) +
    p + 
    # Figure Labels
    labs(
      title = labels[["title"]],
      subtitle = labels[["subtitle"]],
      y = labels[["y"]],
      x = labels[["x"]],
      caption = labels[["caption"]]
    ) +
    scale_y_continuous(limits = scales[["y"]][["limits"]], label = scales::label_number(), position = scales[["y"]][["position"]]) +
    scale_x_discrete(position = scales[["x"]][["position"]]) +
    theme_custom() -> p
  
    if (coord_flip) {
      p <- p + coord_flip()
    }
    
    return(p)
    
}

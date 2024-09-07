######################################################
# Visualization functions -----------------------
######################################################

# Generic function to prepare estimates for plotting
prep_fig_data <- function(ests, factor_params, include_valuelabel = TRUE, valuelabel_target = "mean", valuelabel_type = "pct", valuelabel_thresh = NULL) {
  
  wrap_sizes <- factor_params[["wrap_sizes"]]
  order_vars <- factor_params[["order_vars"]]
  reverse_order <- factor_params[["reverse_order"]]
  
  ests %>%
    mutate(
      indicator_group = style_factors(indicator_group, wrap_sizes[["indicator_group"]], reverse_order[["indicator_group"]], order_vars[["indicator_group"]]), 
      group_name = style_factors(group_name, wrap_sizes[["group_name"]], reverse_order[["group_name"]], order_vars[["group_name"]]), 
    ) %>% 
    group_by(indicator_group) %>% 
    mutate(
      indicator_name = style_factors(indicator_name, wrap_sizes[["indicator_name"]], reverse_order[["indicator_name"]], order_vars[["indicator_name"]])
    ) %>%
    group_by(group_name) %>% 
    mutate(
      group_cat_val = style_factors(group_cat_val, wrap_sizes[["group_cat_val"]], reverse_order[["group_cat_val"]], order_vars[["group_cat_val"]])
    ) %>% 
    ungroup() %>%
    filter(!is.na(group_cat_val)) %>% 
    filter(group_cat_val != "Don't know") -> fig_data
   
  if (include_valuelabel) { 
    value = sym(valuelabel_target)
    if (valuelabel_type == "pct") {
      fig_data %>% mutate(valuelabel = paste0(pctclean(!!value, 0), "%")) -> fig_data
      if (!is.null(valuelabel_thresh)) { 
        fig_data %>% mutate(valuelabel = ifelse(!!value < valuelabel_thresh, NA, valuelabel)) -> fig_data
        }
    } else { 
      fig_data %>% mutate(valuelabel = paste0(numclean(!!value))) -> fig_data
      if (!is.null(valuelabel_thresh)) { 
        fig_data %>% mutate(valuelabel = ifelse(!!value < valuelabel_thresh, NA, valuelabel)) -> fig_data
      }
      }
    }
  
  return(fig_data)
  
}

# Generic function to prepare estimates for plotting
prep_reg_factors <- function(ests, factor_params, include_valuelabel = TRUE) {
  
  wrap_sizes <- factor_params[["wrap_sizes"]]
  order_vars <- factor_params[["order_vars"]]
  reverse_order <- factor_params[["reverse_order"]]
  
  ests %>%
    mutate(
      model_type = style_factors(model_type, wrap_sizes[["model_type"]], reverse_order[["model_type"]], order_vars[["model_type"]]), 
    ) %>% 
    group_by(model_type) %>% 
    mutate(
      depvar_label = style_factors(depvar_label, wrap_sizes[["depvar_label"]], reverse_order[["depvar_label"]], order_vars[["depvar_label"]])
    ) %>%
    group_by(model_type, depvar_label) %>% 
    mutate(
      effect_label = style_factors(effect_label, wrap_sizes[["effect_label"]], reverse_order[["effect_label"]], order_vars[["effect_label"]])
    ) %>% 
    ungroup() -> fig_data
  
  if (include_valuelabel) { 
    fig_data %>% mutate(valuelabel = paste0(pctclean(estimate, 0), "%")) -> fig_data
  }
  
  return(fig_data)
  
}

prep_reg_data <- function(data) {
  
  data %>%
    group_by(depvar, confounds_flag) %>%
    mutate(
      
      baseline = max(ifelse(term == "(Intercept)", estimate, NA), na.rm = TRUE),
      baseline = ifelse(row_number() == 1, NA, baseline), 
      
      sig = ifelse(p.value < 0.1, ".", NA),
      sig = ifelse(p.value < 0.05, "*", sig),
      sig = ifelse(p.value < 0.01, "**", sig),
      sig = ifelse(p.value < 0.001, "***", sig),
      sig = ifelse(is.na(sig), "", sig),
      
      valuelabel = numclean(estimate, n = 2),
      
      effect_dir = ifelse(estimate < 0, -1, 1),
      
      startarrow = baseline,
      endarrow = fig_data,
      
      valuelabel = paste0(valuelabel, sig),
      valuelabel = ifelse(term == "(Intercept)", NA, valuelabel),
      
      valuelabel_pos = ifelse(effect_dir == 1, (endarrow - startarrow)/2 + startarrow, (startarrow - endarrow)/2 + endarrow),
      
      barlabel =  numclean(fig_data, n = 2),
      annotation = paste("R-sqrd:", round(adj_rsquared, 3), sep = " "),
      annotation = ifelse(term == "(Intercept)", NA, annotation)
    )
  
}

prep_reg_data_allcountries <- function(data) {
  
  data %>%
    group_by(country, depvar, confounds_flag) %>%
    mutate(
      
      baseline = max(ifelse(term == "(Intercept)", estimate, NA), na.rm = TRUE),
      baseline = ifelse(row_number() == 1, NA, baseline), 
      
      sig = ifelse(p.value < 0.1, ".", NA),
      sig = ifelse(p.value < 0.05, "*", sig),
      sig = ifelse(p.value < 0.01, "**", sig),
      sig = ifelse(p.value < 0.001, "***", sig),
      sig = ifelse(is.na(sig), "", sig),
      
      valuelabel = numclean(estimate, n = 2),
      
      effect_dir = ifelse(estimate < 0, -1, 1),
      
      startarrow = baseline,
      endarrow = fig_data,
      
      valuelabel = paste0(valuelabel, sig),
      valuelabel = ifelse(term == "(Intercept)", NA, valuelabel),
      
      valuelabel_pos = ifelse(effect_dir == 1, (endarrow - startarrow)/2 + startarrow, (startarrow - endarrow)/2 + endarrow),
      
      barlabel =  numclean(fig_data, n = 2),
      annotation = paste("R-sqrd:", round(adj_rsquared, 3), sep = " "),
      annotation = ifelse(term == "(Intercept)", NA, annotation), 
      
      city = CITIES[country]
      
    )
  
}


fig_bar <- function(base_plot, data, params) {
  
  # Geoms
  if (params[["bars"]][["position"]] == "stack") {
    pos <- position_stack(vjust = params[["valuelabels"]][["lab_vjust"]])
  } else if (params[["bars"]][["position"]] == "dodge") {
    pos <- position_dodge(width = params[["bars"]][["position_width"]])
  }
  
  fill = params[["bars"]][["fill"]]
  
  if (!is.null(fill)) {
    p <- base_plot + geom_col(width =  params[["bars"]][["width"]], color = params[["bars"]][["color"]], fill = params[["bars"]][["fill"]], position = pos)
  } else { 
    p <- base_plot + geom_col(width =  params[["bars"]][["width"]], color = params[["bars"]][["color"]], position = pos)
  }
  
  # Value labels
  if (params[["valuelabels"]][["show"]]) {
    # Reguires variable "valuelabel" to be defined in the dataset
    p <- p +
      geom_text(aes(label = valuelabel), position = pos, size = params[["valuelabels"]][["lab_size"]], hjust = params[["valuelabels"]][["lab_hjust"]], fontface = params[["valuelabels"]][["lab_face"]])
  }
  
  # Error bars
  if (params[["errorbars"]][["show"]]) {
    
    ymin <- sym(names(data)[str_detect(names(data), "_upp")])
    ymax <- sym(names(data)[str_detect(names(data), "_low")])
    p <- p + geom_linerange(aes(ymin = !!ymin, ymax = !!ymax), position = pos, color = params[["errorbars"]][["color"]])
    
  }
  
  if (params[["catlabels"]][["show"]]) {
    # Reguires variable "catlabel" to be defined in the dataset
    p <- p + geom_text(aes(y = 0, label = catlabel), position = pos, hjust = 0, size = 2.5, fontface = "bold")
  }
  
  if (params[["bars"]][["labeltotal"]]) {
    # Requires barlabelpos and barlabel variables in data
    if (params[["bars"]][["labeltotal_ndgy"]]) {
      maxy <- max(data[["barlabelpos"]], na.rm = TRUE)
      ndg_y <- maxy/60
    } else {
      ndg_y = 0
    }
    p <- p +
      geom_text(aes(y = barlabelpos, label = barlabel), hjust = 0, nudge_y = ndg_y, fontface = "bold") 
  }
  
  return(p)
  
}

fig_tile <- function(base_plot, params) { 
  
  # Geoms
  
  p <- base_plot + geom_tile(color = params[["tiles"]][["color"]])
  
  # Value labels
  if (params[["valuelabels"]][["show"]]) {
    p <- p +
      geom_text(aes(label = valuelabel), 
                size = params[["valuelabels"]][["lab_size"]], 
                hjust = params[["valuelabels"]][["lab_hjust"]], 
                fontface = params[["valuelabels"]][["lab_face"]])
  }
  
  return(p)
  
}

fig_flex <- function(data, vars, facets, params, scales, legend, labels, coord_flip = FALSE) {
  
  # Variables to plot -------
  xvar <- sym(vars[["xvar"]])
  yvar <- sym(vars[["yvar"]])
  
  if (!is.null(vars[["fillvar"]])) {
    fvar <- sym(vars[["fillvar"]])
  } else { 
    fvar = NULL
  }
  
  # Defining base_plot  -------
  base_plot <- ggplot(
    data = data,
    aes(
      x = !!xvar,
      y = !!yvar,
      fill = !!fvar
    )
  )
  
  # Adding geoms ------
  if (params[["geom_type"]] == "bar") { 
    p <- fig_bar(base_plot, data, params)
  } else if (params[["geom_type"]] == "tile") { 
    p <- fig_tile(base_plot, params)
  }
  
  # Adding facets ------
  if (!is.null(facets)) {
    
    fcvar_rows <- sym(facets[["rows"]])
    fcvar_cols <- sym(facets[["cols"]])
    
    if (facets[["type"]] == "wrap") {
      p <- p + facet_wrap(
        vars(!!fcvar_cols), 
        nrow = facets[["wrap_nrows"]], 
        ncol = facets[["wrap_ncols"]], 
        scales = facets[["scales"]])
    } else if (facets[["type"]] == "grid") {
      p <- p + facet_grid(
        rows = vars(!!fcvar_rows), 
        cols = vars(!!fcvar_cols), 
        scales = facets[["scales"]], 
        switch = "y", 
        space = facets[["space"]])
    }
    
  }
  
  # Legend
  if (legend[["show"]]) { 
    p <-  p + guides(
      fill = guide_legend(
        title = legend[["title"]], 
        direction = legend[["direction"]], 
        nrow = legend[["nrows"]], 
        reverse = legend[["reverse"]])
    ) 
  } else { 
    p <- p + guides(fill = "none")
  }
  
  # Scales ------------
  
  # Fill color
  if (params[["geom_type"]] == "bar") {
    if (!is.null(scales[["fillcolor"]][["palette"]])) {
      p <- p + scale_fill_manual(values = scales[["fillcolor"]][["palette"]])
    }
  } 
  
  else if (params[["geom_type"]] == "tile") {
  if (!is.null(scales[["fillcolor"]][["palette"]])) {
    if(scales[["fillcolor"]][["palette"]] == "identity") { 
      p <- p + scale_fill_identity()
    }
    else {
      p <- p + scale_fill_distiller(palette = scales[["fillcolor"]][["palette"]], direction = scales[["fillcolor"]][["direction"]] )
    }
  }
  }
  
  # Y-Axis
  
  if (params[["geom_type"]] == "bar") {
    
    if (scales[["yaxis"]][["type"]] == "number") {
      if (params[["bars"]][["labeltotal"]]) {
        if (params[["bars"]][["labeltotal_extendmax"]]) {
        maxy <- max(data[["barlabelpos"]], na.rm = TRUE)
          p <- p +  scale_y_continuous(position = "right", limits = c(0, maxy + 0.15*maxy), labels = scales::label_comma(), expand = scales[["yaxis"]][["expand"]] )
        }
      } else { 
        p <- p +  scale_y_continuous(position = "right", labels = scales::label_comma(), scales[["yaxis"]][["expand"]] )
      }
    }
  
    if (scales[["yaxis"]][["type"]] == "percent") {
      limits <- scales[["yaxis"]][["limits"]]
      nbreaks <- scales[["yaxis"]][["nbreaks"]]
      breaks <- seq(limits[1], limits[2], by = (limits[2] - limits[1])/nbreaks)
      p <- p +  scale_y_continuous(position = "right", limits = limits, breaks = breaks, labels = scales::label_percent(suffix = "")) 
      if (scales[["yaxis"]][["droplines"]]) { 
        p <- p + geom_hline(yintercept = breaks, color ="white")
      } 
    } 
  
  } else { 
    
    p <- p + scale_y_discrete(position = "right")
    
  }
  
  # Figure Labels ------
  p <- p + 
    labs(
      title = labels[["title"]],
      subtitle = labels[["subtitle"]],
      y = labels[["yax_ti"]],
      x = labels[["xax_ti"]],
      caption = labels[["caption"]]
    ) +
    theme_custom()
  
  # Flip coordinates?
  if (coord_flip) {
    p <- p + coord_flip()
  }
  
  if (!is.null(facets)) {
    # Final touches
    if (facets[["drop_row_label"]]) { 
      p <- p + theme(strip.text.y.left = element_blank())
    }
    
    if (facets[["drop_col_label"]]) { 
      p <- p + theme(strip.text.x.left = element_blank())
    }
    
    if (facets[["add_dividers"]]) { 
      p <- p + theme(panel.background = element_rect(fill = NA, linetype = "solid", color = "grey75"))  
    }
  } 
  
  return(p)
  
}

fig_regests <- function(data, labels, facets, barparams, scales, coord_flip = FALSE) {
  
  p <- ggplot(data = data,
         aes(x = effect_label,
             y = fig_data))
    
    if (!is.null(facets)) {
      fcvar_rows <- sym(facets[["rows"]])
      fcvar_cols <- sym(facets[["cols"]])
      
      if (facets[["type"]] == "wrap") {
        
        p <- p + facet_wrap(vars(!!fcvar_cols), nrow = 1, scales = facets[["scales"]])
        
      } else if (facets[["type"]] == "grid") {
        
        p <- p + facet_grid(rows = vars(!!fcvar_rows), cols = vars(!!fcvar_cols), scales = facets[["scales"]], switch = "y", space = facets[["space"]])
        
      }
      
    }
  
    p + geom_col(width =  barparams[["bars"]][["width"]], fill = barparams[["bars"]][["color"]]) + 
    geom_hline(aes(yintercept = baseline), color = "red", size = 0.25, linetype = "dashed") + 
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
    
    if (!is.null(facets)) {
      # Final touches
      if (facets[["drop_row_label"]]) { 
        p <- p + theme(strip.text.y.left = element_blank())
      }
    }
    
    return(p)
    
}







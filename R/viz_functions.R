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
    p <- p +  scale_y_continuous(labels = scales::label_comma())
  }
  else if (yaxtype == "percent") {
    p <- p +  scale_y_continuous(labels = scales::label_percent())
  }

  if (legend == FALSE) {
    p <- p + theme(legend.position = "none")
  }

  return(p)

}

fig_bar_w_fill <- function(data, xvar, yvar, fvar, facets, labels, barparams, palette = NULL,  coord_flip = FALSE, yaxtype = "number", legend = TRUE, errorbars = FALSE) {

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
    pos <- position_stack(vjust = 0.5)
  } else if (barparams[["bars"]][["position"]] == "dodge") {
    pos <- position_dodge(width = barparams[["bars"]][["position_width"]])
  }

  p <- p + geom_col(width =  barparams[["bars"]][["width"]], position = pos)

  # Value labels
  if (barparams[["valuelabels"]][["show"]]) {
    p <- p +
      geom_text(aes(label = valuelabel), position = pos, hjust = barparams[["valuelabels"]][["lab_hjust"]], fontface = barparams[["valuelabels"]][["lab_face"]])
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
  p <-  p + guides(fill = guide_legend(title = labels[["legend_ti"]], nrow = 1)) +

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

if (barparams[["bars"]][["labeltotal"]]) {
  maxy <- max(data[["barlabelpos"]], na.rm = TRUE)
  ndg_y <- maxy/45
  p <- p +
    geom_text(aes(y = barlabelpos, label = barlabel), hjust = 0, nudge_y = ndg_y, fontface = "bold") +
    scale_y_continuous(limits = c(0, maxy + 0.1*maxy))
}

if (yaxtype == "number") {
  p <- p +  scale_y_continuous(labels = scales::label_comma())
}
else if (yaxtype == "percent") {
  p <- p +  scale_y_continuous(labels = scales::label_percent())
}
if (legend == FALSE) {
  p <- p + theme(legend.position = "none")
}

return(p)

}



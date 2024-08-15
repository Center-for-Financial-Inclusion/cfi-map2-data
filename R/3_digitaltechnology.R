
########################################################################

# Conducting analysis for Part 3 of report --------------------------

# 1.	What is the mindset and profile of the people who own and operate small firms?
# a.	Age, gender and years of education of the owner, how does the distribution compare to the population overall (compare against census data)?
# b. How do owners differ with respect to their motivations and attitudes to growth and risk? Is gender or years of education associated with a growth-oriented mindset or is this a psychological trait that is mostly independent of socio-economic characteristics?

#####################################################################################


# Figure 10  -----------------------

fig_10_data <- function(ests, indicators) {

  ests %>%
    mutate(
      indicator_name = fct_rev(factor(indicator_name, levels = indicators, ordered = TRUE)),
      valuelabel = paste0(pctclean(mean, 0), "%"),
      indicator_group = "Connectivity and device usage",
      catlabel = ifelse(indicator == "tech_has_internet", group_cat_val, NA),
      #group_cat_val = factor(group_cat_val, levels = GROUP_CAT_LEVELS, ordered = TRUE)
    ) %>%
    filter(!is.na(group_cat_val))

}


# Figure 11  -----------------------

fig_11_data <- function(ests, indicators) {

  indicators = unique(str_trim(str_replace(indicators, "\\(30-day active\\)", "")))

  ests %>%
    mutate(
      #indicator_name = fct_rev(factor(indicator_name, levels = indicators, ordered = TRUE)),
      mean = ifelse(indicator == "tech_function_efin_30da", NA, mean),
      valuelabel = paste0(pctclean(mean, 0), "%"),
      user_group = ifelse(str_detect(indicator, "30da"), "30-day active", "Uses"),
      indicator_name = str_trim(str_replace(indicator_name, "\\(30-day active\\)", "")),
      indicator_name = fct_rev(factor(indicator_name, levels = indicators, ordered = TRUE)),
      indicator_group = "Digital technology usage"
      #group_cat_val = factor(group_cat_val, levels = GROUP_CAT_LEVELS, ordered = TRUE)
    ) %>%
    filter(!is.na(group_cat_val))

}


# Figure 12  -----------------------

fig_12_data <- function(ests, indicators) {

  ests %>%
    mutate(
      indicator_name = fct_rev(factor(indicator_name, levels = indicators, ordered = TRUE)),
      valuelabel = paste0(pctclean(mean, 0), "%"),
      catlabel = ifelse(indicator == "tech_cat_none", group_cat_val, NA),
      indicator_group = "Digital technology usage",
      #group_cat_val = factor(group_cat_val, levels = GROUP_CAT_LEVELS, ordered = TRUE)
    ) %>%
    filter(!is.na(group_cat_val))

}

# Figure 13  -----------------------

fig_13_data <- function(data) {

  data %>%
    group_by(depvar, confounds_flag) %>%
    mutate(

      baseline = max(ifelse(term == "(Intercept)", estimate, NA), na.rm = TRUE),

      sig = ifelse(p.value < 0.1, ".", NA),
      sig = ifelse(p.value < 0.05, "*", sig),
      sig = ifelse(p.value < 0.01, "**", sig),
      sig = ifelse(p.value < 0.001, "***", sig),
      sig = ifelse(is.na(sig), "", sig),

      valuelabel = numclean(estimate, n = 2),

      effect_dir = ifelse(estimate < 0, -1, 1),

      startarrow = baseline,
      endarrow = fig_data,

      valuelabel = paste0("$", valuelabel, sig),
      valuelabel = ifelse(term == "(Intercept)", NA, valuelabel),

      valuelabel_pos = ifelse(effect_dir == 1, (endarrow - startarrow)/2 + startarrow, (startarrow - endarrow)/2 + endarrow),

      barlabel =  numclean(fig_data, n = 2),
      annotation = paste("R-sqrd:", round(adj_rsquared, 3), sep = " "),
      annotation = ifelse(term == "(Intercept)", NA, annotation)
    )

}

fig_13_chart <- function(data, labels) {

  ggplot(data = data,
         aes(x = effect_label,
             y = fig_data)
  ) +
    facet_grid(cols = vars(depvar_label), rows = vars(model_type), switch = "y") +
    geom_col(width = 0.5, fill = "grey80") +
    geom_segment(aes(y = startarrow, yend = endarrow), color = "red", arrow = arrow(length=unit(.2, 'cm'))) +
    geom_point(aes(y = startarrow), shape = 21, color = "red", fill = "white") +
    geom_text(aes(label = barlabel), vjust = 0, nudge_y = 0.05, color = "black") +
    geom_text(aes(y = valuelabel_pos, label = valuelabel), color = "red", hjust = 0, nudge_x = 0.05) +
    #geom_text(aes(y = 0.95, label = annotation), hjust = 0, color = "black", nudge_x = 0, size = 3.5) +
    # Figure Labels
    labs(
      title = labels[["title"]],
      subtitle = labels[["subtitle"]],
      y = labels[["yax_ti"]],
      x = labels[["xax_ti"]],
      caption = labels[["caption"]]
    ) +
    scale_y_continuous(label = scales::label_number()) +
    theme_custom()

}

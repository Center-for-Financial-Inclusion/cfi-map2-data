
########################################################################

# Conducting analysis for Part 2 of report --------------------------

# 1.	What is the mindset and profile of the people who own and operate small firms?
# a.	Age, gender and years of education of the owner, how does the distribution compare to the population overall (compare against census data)?
# b. How do owners differ with respect to their motivations and attitudes to growth and risk? Is gender or years of education associated with a growth-oriented mindset or is this a psychological trait that is mostly independent of socio-economic characteristics?

#####################################################################################


# Figure 5 Socio-demographic Characteristics of MSMEs -----------------------

fig_5_data <- function(ests, indicators, wrap_len = 40) {

  ests %>%
    mutate(
      valuelabel = paste0(pctclean(mean, 0), "%"),
      indicator_name = fct_rev(factor(str_wrap(indicator_name, wrap_len), levels = str_wrap(indicators, wrap_len), ordered = TRUE)),
      indicator_group = ifelse(str_detect(indicator, "education"), "Education completed", NA),
      indicator_group = ifelse(str_detect(indicator, "experience"), "Experience running business", indicator_group),
      indicator_group = ifelse(str_detect(indicator, "age"), "Age", indicator_group)
    )

}

# Figure 6 Pyschographic characteristics of MSMEs owners/managers -----------------------

fig_6_data <- function(ests, indicators, wrap_len = 40) {

  ests %>%
    mutate(
      valuelabel = paste0(pctclean(mean, 0), "%"),
      indicator_name = fct_rev(factor(str_wrap(indicator_name, wrap_len), levels = str_wrap(indicators, wrap_len), ordered = TRUE)),
      indicator_group = ifelse(str_detect(indicator, "motivation"), "1. Reason for starting business", NA),
      indicator_group = ifelse(str_detect(indicator, "maingoal"), "2. Primary goal for business", indicator_group),
      indicator_group = ifelse(str_detect(indicator, "riskapproach"), "3. Approach to risk", indicator_group),
      indicator_group = ifelse(str_detect(indicator, "attitude"), "4. Attitude facing past challenges", indicator_group), 
      indicator_group = ifelse(str_detect(indicator, "segment"), "4. Pyschographic segment", indicator_group)
      #indicator_group = factor(indicator_group, levels = c("Reason for starting business", "Primary goal for business", "Approach to risk", "Pyschographic segment"), ordered = TRUE)
    )

}

# Figure 7 Correlation plot between  -----------------------

fig_7_data <- function(corr) {

  levels <- c("Desire to have own business", "Lack of other income opportunities or desire to generate additional income", "Inherited business or other reason", "Grow the business", "Cover business costs", "Leave business to get a regular wage paying job or other", "Aggresively pursue risky opportunities that could generate significant additional income", "Cautiously pursue risk opportunities that could generate a little income", "Avoid risks even if they could generate additional income")

  flattenCorrMatrix(corr$r, corr$P) %>%
    mutate(
      row_name = INDICATORS[row],
      col_name = INDICATORS[column],
      row_group = ifelse(str_detect(row, "motivation"), "Reason for starting business", NA),
      row_group = ifelse(str_detect(row, "maingoal"), "Primary goal for business", row_group),
      row_group = ifelse(str_detect(row, "riskapproach"), "Approach to risk", row_group),
      col_group = ifelse(str_detect(row, "motivation"), "Reason for starting business", NA),
      col_group = ifelse(str_detect(row, "maingoal"), "Primary goal for business", col_group),
      col_group = ifelse(str_detect(row, "riskapproach"), "Approach to risk", col_group),
      sig = ifelse(p < 0.05, 1, 0),
      alpha = ifelse(sig == 1, 1, 0.6),
      valuelabel = ifelse(sig == 1, paste0(round(cor, 2), "*"), round(cor, 2)),
      color = ifelse(sig == 1, "black", "grey70"),
      drop = ifelse(col_name == "Cover business costs" & row_name %in% c("Grow the business", "Leave business to get a regular wage paying job or other"), 1, 0),
      drop = ifelse(col_name == "Desire to have own business" & row_name %in% c("Lack of other income opportunities or desire to generate additional income", "Inherited business or other reason"), 1, drop),
      drop = ifelse(col_name == "Inherited business or other reason" & row_name %in% c("Lack of other income opportunities or desire to generate additional income", "Desire to have own business"), 1, drop),
      drop = ifelse(col_name == "Leave business to get a regular wage paying job or other" & row_name %in% c("Grow the business"), 1, drop),
      drop = ifelse(col_name == "Lack of other income opportunities or desire to generate additional income" & row_name == "Desire to have own business", 1, drop),
      row_name = factor(str_wrap(row_name, 40), levels = str_wrap(levels, 40), ordered = TRUE),
      col_name = factor(str_wrap(col_name, 15), levels = str_wrap(levels, 15), ordered = TRUE),
      col_group = factor(col_group, levels = c("Reason for starting business", "Primary goal for business"), ordered = TRUE)
    ) %>%
    filter(drop != 1) %>%
    filter(col_group != "Approach to risk")

}

fig_7_chart <- function(data) {

  ggplot(data = data,
         aes(x = col_name,
             y = fct_rev(row_name),
             fill = cor)
  ) +
    facet_grid(rows = vars(col_group), scales = "free", switch = "y") +
    geom_tile(aes(alpha = alpha)) +
    geom_text(aes(label = valuelabel)) +
    geom_vline(xintercept = 3.5, color = "black") +
    scale_x_discrete(position = "top") +
    scale_alpha_identity() +
    scale_fill_distiller(palette = "RdBu", direction = 1) +
    theme_custom() +
    labs(
      y = NULL,
      x = NULL,
      title = "The relationship between MSE owner's motivations, goals and attitudes to risk",
      subtitle = 'Correlation coefficient',
      caption = 'Notes: Correlation coefficients significant with a p-value of 0.05 or less are labeled with an *.'
    ) +
    theme(strip.text.y = element_text(face = "bold"), legend.position = "none")


}

# Figure 8  -----------------------

fig_8_data <- function(data) {

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
      
      valuelabel = paste0(valuelabel, sig, "pp"),
      valuelabel = ifelse(term == "(Intercept)", NA, valuelabel),
      valuelabel = ifelse(sig == "", NA, valuelabel), 
      
      valuelabel_pos = ifelse(effect_dir == 1, (endarrow - startarrow)/2 + startarrow, (startarrow - endarrow)/2 + endarrow),
      
      barlabel =  numclean(fig_data, n = 2),
      annotation = paste("R-sqrd:", round(adj_rsquared, 3), sep = " "),
      annotation = ifelse(term == "(Intercept)", NA, annotation)
    )
}

fig_8_chart <- function(data, labels) {

  data <- data %>% group_by(depvar, model_type) %>% mutate(x = ifelse(term == "(Intercept)", fig_data, NA), refline = max(x, na.rm = TRUE), refline = ifelse(row_number() == 1, refline, NA))
  
  ggplot(data = data,
         aes(x = fct_inorder(str_wrap(effect_label, 15)),
             y = fig_data)
  ) +
    facet_grid(cols = vars(depvar_label), rows = vars(model_type), switch = "y") +
    geom_col(width = 0.55, fill = "grey85") +
    geom_hline(aes(yintercept = refline), color = "red", size = 0.25, linetype = "dashed") + 
    geom_segment(aes(y = startarrow, yend = endarrow), color = "red", arrow = arrow(length=unit(.2, 'cm'), type = "closed"), size = 1.5) +
    geom_point(aes(y = startarrow), shape = 21, color = "red", fill = "white", size= 3.5) +
    #geom_text(aes(label = barlabel), vjust = 0, nudge_y = 0.05, color = "black") +
    geom_text(aes(y = valuelabel_pos, label = valuelabel), color = "red", hjust = 0, nudge_x = 0.05) +
    #geom_text(aes(y = 0.95, label = annotation), hjust = 0, color = "black", nudge_x = 0, size = 3.5) +
    # Figure Labels
    labs(
      title = labels[["title"]],
      subtitle = labels[["subtitle"]],
      y = labels[["y"]],
      x = labels[["x"]],
      caption = labels[["caption"]]
    ) +
    scale_y_continuous(label = scales::label_percent(), position = "left") +
    scale_x_discrete(position = "top") +
    theme_custom()

  }


# Figure 9  -----------------------

fig_9_data <- function(ests, indicators, wrap_len = 40) {

  ests %>%
    mutate(
      valuelabel = paste0(pctclean(mean, 0), "%"),
      indicator_name = fct_rev(factor(str_wrap(indicator_name, wrap_len), levels = str_wrap(indicators, wrap_len), ordered = TRUE)),
      indicator_group = "Business-household co-dependence"
    )

}






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
           max = max(fig_data),
           #max = ifelse(label == "Women", max, NA),
           min = min(fig_data),
           #min = ifelse(label == "Women", min, NA),

           sig = ifelse(p.value < 0.1, ".", NA),
           sig = ifelse(p.value < 0.05, "*", sig),
           sig = ifelse(p.value < 0.01, "**", sig),
           sig = ifelse(p.value < 0.001, "***", sig),
           sig = ifelse(is.na(sig), "", sig),

           maineffect = ifelse(term == "resp_sex_women", estimate, NA),
           valuelabel = round(max(ifelse(effect_label == "Women", estimate, NA), na.rm = TRUE)*100, 1),
           effect_dir = ifelse(valuelabel < 0, -1, 1),

           endarrow = ifelse(effect_dir == 1, max, min),
           startarrow = ifelse(effect_dir == 1, min, max),
           endarrow = ifelse(effect_label == "Women" & effect_dir == 1, NA, endarrow),
           startarrow = ifelse(effect_label == "Women" & effect_dir == 1, NA, startarrow),
           endarrow = ifelse(effect_label == "Men" & effect_dir == -1, NA, endarrow),
           startarrow = ifelse(effect_label == "Men" & effect_dir == -1, NA, startarrow),

           valuelabel = paste0(valuelabel, "pp", sig),
           valuelabel =  ifelse(effect_label == "Women" & effect_dir == 1, NA, valuelabel),
           valuelabel =  ifelse(effect_label == "Men" & effect_dir == -1, NA, valuelabel),

           valuelabel_pos = (max - min)/2 + min,
           barlabel = round(fig_data*100, 1),
           annotation = paste("R-sqrd:", round(adj_rsquared, 3), sep = " "),
           annotation = ifelse(effect_label == "Women", NA, annotation)
    )

}

fig_8_chart <- function(data) {

  ggplot(data = data,
         aes(x = effect_label,
             y = fig_data)
  ) +
    facet_grid(cols = vars(depvar_label), rows = vars(model_type), switch = "y") +
    geom_col(width = 0.5, fill = "grey80") +
    geom_segment(aes(y = startarrow, yend = endarrow), color = "red", arrow = arrow(length=unit(.2, 'cm'))) +
    geom_point(aes(y = startarrow), shape = 21, color = "red", fill = "white") +
    geom_text(aes(label = barlabel), vjust = 1, nudge_y = -0.01, color = "black") +
    geom_text(aes(y = valuelabel_pos, label = valuelabel), color = "red", hjust = 0, nudge_x = 0.07) +
    #geom_text(aes(y = 0.95, label = annotation), hjust = 0, color = "black", nudge_x = 0, size = 3.5) +
    labs(
      title = "Observational estimates of the relationship of owner's gender on the orientation of firms",
      subtitle = "The effect (in percentage points) of female ownership on the likelihood of a particular growth orientation mentality is shown with the red arrow",
      y = "Predicted probability (%)",
      x = "Gender",
      caption = str_wrap("Notes: The results shown are based on a linear regression model that measures the effect of gender
                      on several outcomes of interest. The 'unadjusted' model does not include any additional controls,
                       while the 'adjusted' model includes controls for three variables: years of experience and educational attainment.
                       The regression parameters are used to compute predicted probabilities for male and female business owners with the average years of experience running
                       the business in the sample and with primary-level educational attainment.", 175)
    ) +
    scale_y_continuous(limits = c(0, 1), label = scales::label_percent()) +
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





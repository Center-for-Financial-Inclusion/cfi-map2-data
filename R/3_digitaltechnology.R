
########################################################################

# Conducting analysis for Part 3 of report --------------------------

# 1.	What is the mindset and profile of the people who own and operate small firms?
# a.	Age, gender and years of education of the owner, how does the distribution compare to the population overall (compare against census data)?
# b. How do owners differ with respect to their motivations and attitudes to growth and risk? Is gender or years of education associated with a growth-oriented mindset or is this a psychological trait that is mostly independent of socio-economic characteristics?

#####################################################################################


# Figure 10  -----------------------

fig_10_data <- function(ests) {

  cats <- c("Lo", "Mlo", "Mhi", "Hi")
  blues <- brewer.pal(4, "Blues")
  names(blues) <- cats
  reds <- brewer.pal(4, "Reds")
  names(reds) <- cats

techlevels <- c("Mobile phone/computer/tablet", "Internet connectivity", "Website", "Messaging apps", "Social media", "E-commerce platforms", "Software for operations", "Artificial intelligence", "Merchant digital payments", "Digital loans", "Digital technology adoption score")      
  
  wrap <- 20
  ests %>%
    filter(!is.na(indicator_name)) %>% 
    separate_wider_delim(indicator_name, delim = ": ", names = c("technology", "frequency"), too_few = "align_start") %>% 
    mutate(
      technology = factor(technology, levels = techlevels, ordered = TRUE),
      group_name = str_wrap(group_name, wrap),
      group_cat_val = str_wrap(group_cat_val, wrap), 
      valuelabel = ifelse(technology == "Digital technology adoption score", numclean(mean,1), paste0(round(mean*100, 0), "%")), 
      frequency = factor(str_wrap(frequency, wrap), levels = str_wrap(c("Has", "Uses", "Used in past 12 mos", "Ever used", "Never used", "Stopped using", "Monthly or less", "Daily or weekly", "[0 min -10 max]"), wrap), ordered = TRUE),
      valuecat = case_when(
        mean < 0.25 ~ "Lo", 
        mean >= 0.25 & mean < 0.5 ~ "Mlo", 
        mean >= 0.5 & mean < 0.75 ~ "Mhi", 
        mean >= 0.75 ~ "Hi"),
      valuecat2 = case_when(
        mean < 2.5 ~ "Lo", 
        mean >= 2.5 & mean < 5 ~ "Mlo", 
        mean >= 5 & mean < 7.5 ~ "Mhi", 
        mean >= 7.5 ~ "Hi"),
      fillcolor = ifelse(frequency %in% str_wrap(c("Never used", "Stopped using"), wrap), reds[valuecat], blues[valuecat]), 
      fillcolor = ifelse(technology == "Digital technology adoption score", blues[valuecat2], fillcolor)
      #indicator_name = fct_rev(factor(indicator_name, levels = indicators, ordered = TRUE))
      
    ) %>%
    filter(!is.na(group_cat_val)) %>% 
    filter(group_cat_val != "Don't know")

}

fig_10a_data <- function(data) {
  
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
      
      valuelabel = paste0(valuelabel, sig),
      valuelabel = ifelse(term == "(Intercept)", NA, valuelabel),
      
      valuelabel_pos = ifelse(effect_dir == 1, (endarrow - startarrow)/2 + startarrow, (startarrow - endarrow)/2 + endarrow),
      
      barlabel =  numclean(fig_data, n = 2),
      annotation = paste("R-sqrd:", round(adj_rsquared, 3), sep = " "),
      annotation = ifelse(term == "(Intercept)", NA, annotation)
    )
  
}


# Figure 11  -----------------------

fig_11_data <- function(ests, indicators) {

  indicators = unique(str_trim(str_replace(indicators, "\\(30-day active\\)", "")))

  ests %>%
    mutate(
      #indicator_name = fct_rev(factor(indicator_name, levels = indicators, ordered = TRUE)),
      valuelabel = paste0(pctclean(mean, 0), "%"),
      user_group = ifelse(str_detect(indicator, "30da"), "30-day active", ifelse(str_detect(indicator, "7da"), "7-day active", "Uses")),
      user_group = fct_rev(factor(user_group, levels = c("Uses", "30-day active", "7-day active"), ordered = TRUE)), 
      indicator_name = str_trim(str_replace(indicator_name, "\\(30-day active\\)", "")),
      indicator_name = str_trim(str_replace(indicator_name, "\\(7-day active\\)", "")),
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

# Figure 12a  -----------------------

fig_12a_data <- function(ests, indicators) {
  
  ests %>%
    separate_wider_delim(indicator_name, delim = ": ", names = c("indicator_group", "indicator_name"), too_few = "align_start") %>% 
    mutate(
      indicator_group = case_match(indicator_group, 
         "Adoption factors" ~ "1. Adoption factors", 
        "Adoption benefits" ~ "2. Adoption benefits", 
        "Adoption challenges" ~ "3. Adoption challenges"
      ), 
      indicator_name = fct_reorder(indicator_name, mean), 
      valuelabel = paste0(pctclean(mean, 0), "%"), 
      group_cat_val = fct_inorder(group_cat_val)
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

  data <- data %>% group_by(model_type) %>% mutate(x = ifelse(term == "(Intercept)", fig_data, NA), refline = max(x, na.rm = TRUE), refline = ifelse(row_number() == 1, refline, NA))
  
  ggplot(data = data,
         aes(x = fct_inorder(str_wrap(effect_label, 15)),
             y = fig_data)
  ) +
    facet_grid(cols = vars(model_type), rows = vars(depvar_label), switch = "y") +
    geom_col(width = 0.55, fill = "grey85") +
    geom_hline(aes(yintercept = refline), color = "red", size = 0.25, linetype = "dashed") + 
    geom_segment(aes(y = startarrow, yend = endarrow), color = "red", arrow = arrow(length=unit(.2, 'cm'), type = "closed"), size = 1.5) +
    geom_point(aes(y = startarrow), shape = 21, color = "red", fill = "white", size= 3.5) +
    geom_text(aes(label = barlabel), vjust = 0, nudge_y = 0.05, color = "black") +
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
    scale_y_continuous(label = scales::label_number(), position = "left") +
    scale_x_discrete(position = "top") +
    theme_custom()

}


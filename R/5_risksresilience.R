########################################################################

# Conducting analysis for Part 5 of report --------------------------

# 1.	What is the mindset and profile of the people who own and operate small firms?
# a.	Age, gender and years of education of the owner, how does the distribution compare to the population overall (compare against census data)?
# b. How do owners differ with respect to their motivations and attitudes to growth and risk? Is gender or years of education associated with a growth-oriented mindset or is this a psychological trait that is mostly independent of socio-economic characteristics?

#####################################################################################


fig_14_data <- function(ests) {
  
  ests %>%
    mutate(
      indicator_name = fct_reorder(indicator_name, mean), 
      biggest_bar = ifelse(max(mean, na.rm = TRUE) == mean, 1, 0), 
      catlabel = ifelse(biggest_bar ==1, group_cat_val, NA),
      valuelabel = paste0(pctclean(mean, 0), "%"), 
      indicator_group = "Main risk faced in past 36 months"
    ) %>%
    filter(!is.na(group_cat_val)) %>% 
    filter(group_cat_val != "Don't know")
  
}


fig_15_data <- function(ests) {
  
  ests %>%
    mutate(
      indicator_group = ifelse(str_detect(indicator, "impact"), "How did weather risk impact business operations?", NA), 
      indicator_group = ifelse(str_detect(indicator, "cope"), "What actions did the business take to cope with the impacts of this weather risk?", indicator_group), 
      indicator_group = ifelse(str_detect(indicator, "cond"), "What is the condition of the business now, compared to before the weather risk occurred?", indicator_group), 
      indicator_group = fct_inorder(str_wrap(indicator_group, 30)), 
      indicator_name = fct_rev(fct_inorder(indicator_name)), 
      valuelabel = paste0(pctclean(mean, 0), "%")
    ) %>%
    filter(!is.na(group_cat_val))
  
}

fig_16_data <- function(ests) {
  
  ests %>%
    mutate(
      indicator_group = ifelse(str_detect(indicator, "firm_diff"), "Difficulty coming up with emergency funds in the next 7 days", NA), 
      indicator_group = ifelse(str_detect(indicator, "firm_source"), "Main source of emergency funds", indicator_group), 
      indicator_group = ifelse(str_detect(indicator, "cust_diff"), "Difficulty coping with loss of main customer or supplier", indicator_group), 
      indicator_group = fct_inorder(str_wrap(indicator_group, 90)), 
      indicator_name = fct_rev(fct_inorder(indicator_name)), 
      valuelabel = paste0(pctclean(mean, 0), "%")
    ) %>%
    filter(!is.na(group_cat_val))
  
}

fig_17_data <- function(ests) {
  
  ests %>%
    mutate(
      indicator_group = ifelse(str_detect(indicator, "resi_efunds_hh_owner"), "Would owner need take resources from business to cover a household emergency?", NA), 
      indicator_group = ifelse(str_detect(indicator, "suppchain_cust"), "How difficult would the loss of main customer or supplier be?", indicator_group), 
      indicator_group = ifelse(str_detect(indicator, "network_conf"), "If you needed support to sustain business, how confident are you in your network?", indicator_group), 
      indicator_group = fct_inorder(str_wrap(indicator_group, 30)), 
      indicator_name = fct_rev(fct_inorder(indicator_name)), 
      valuelabel = paste0(pctclean(mean, 0), "%")
    ) %>%
    filter(!is.na(group_cat_val))
  
}


# Figure 10  -----------------------

fig_18_data <- function(ests, indicators) {
  
  blues <- brewer.pal(4, "Blues")
  reds <- brewer.pal(4, "Reds")
  palette <- c(rev(reds), blues)
  names(palette) <- seq(1, 8, 1)
  
  wrap <- 20
  ests %>%
    filter(!is.na(indicator_name)) %>% 
    mutate(
      indicator_type = ifelse(indicator %in% c("resi_capital_score_v1", "resi_capital_score_v2"), "cont", "disc"), 
      indicator_group = "Resilience capital", 
      indicator_name = fct_rev(factor(indicator_name, levels = indicators, ordered = TRUE)), 
      group_name = str_wrap(group_name, wrap),
      group_cat_val = str_wrap(group_cat_val, wrap), 
      valuelabel = ifelse(indicator %in% c("resi_capital_score_v1", "resi_capital_score_v2"), numclean(mean,1), paste0(round(mean*100, 0), "%")), 
      valuecat = ifelse(indicator %not_in% c("resi_capital_score_v1", "resi_capital_score_v2"), cut(mean, breaks = seq(1,9,1)/8, labels = seq(1,8,1)), NA), 
      valuecat = ifelse(indicator %in% c("resi_capital_score_v1"), cut(mean, breaks = (seq(1,9,1)/8)*4, labels = seq(1,8,1)), valuecat), 
      valuecat = ifelse(indicator %in% c("resi_capital_score_v2"), cut(mean, breaks = (seq(1,9,1)/8)*3, labels = seq(1,8,1)), valuecat), 
      fillcolor = palette[valuecat] 
    ) %>%
    filter(!is.na(group_cat_val)) %>% 
    filter(group_cat_val != "Don't know")
  
}


########################################################################
# Specific functions: T
# These are functions used to produce data for figures that cannot be handled by the generic functions
########################################################################

########################################################################
# 1. Business landscape ------------------------------------
########################################################################

total_businesses <- function(ests) {
  
  ests %>%
    ungroup() %>%
    group_by(business_size_agg2) %>%
    mutate(
      size_total = sum(total),
      share = total/size_total,
      valuelabel = paste0(numclean(total), "\n (", pctclean(share, 0), "%)"),
      valuelabel = ifelse(total < 15000, NA, valuelabel),
      barlabel = ifelse(group_cat_val == "Services: transportation, construction, repair, other", numclean(size_total), NA),
      barlabelpos = ifelse(group_cat_val == "Services: transportation, construction, repair, other", size_total, NA)
    ) %>%
    filter(group_cat_val != "Don't know")
  
}

total_businesses_title <- function(ests, fig_data) {
  
  # Values for title
  global_total <- numclean(sum(ests$total))
  global_total_low <- numclean(sum(ests$total_low))
  global_total_upp <- numclean(sum(ests$total_upp))
  margin <- pctclean(sum(ests$total_upp)/sum(ests$total) - 1, 0)
  pct_small <- pctclean(pull(fig_data %>%
                               group_by(business_size_agg2) %>%
                               summarise(total = sum(total)) %>%
                               ungroup() %>%
                               mutate(share = total/sum(total)) %>%
                               filter(business_size_agg2 == "1-10 people"),
                             share), 0)
  
  title <- glue('An estimated {global_total} (+/- {margin}%) businesses operate in the {CITY} study area. Of these {pct_small}% have 10 or fewer regular employees.')
  return(title)
  
}


sample_chars <- function(ests, groups) {
  
  ests %>%
    group_by(group) %>%
    mutate(
      size_total = sum(total),
      share = total/size_total
    ) %>% ungroup() %>%
    mutate(
      group_name = fct_rev(factor(group_name, levels = groups, ordered = TRUE)),
      group_cat_val = ifelse(group_cat_val == "Household premises" , "Household", group_cat_val),
      group_cat_val = ifelse(group_cat_val == "Non-household premises with permanent structure", "Non-household: Permanent structure", group_cat_val),
      group_cat_val = ifelse(group_cat_val == "Non-household premises with semi-permanent structure, including stalls or stands", "Non-household: Semi-permanent structure", group_cat_val),
      group_cat_val = ifelse(is.na(group_cat_val), "Don't know", group_cat_val),
      valuelabel = paste0(str_wrap(group_cat_val, 30)),
      #valuelabel = paste0(str_wrap(group_cat_val, 20), "\n", pctclean(share, 0), "%"),
    ) %>%
    filter(!is.na(group_cat_val) | group_cat_val != "Don't know")
  
}

# Share of businesses by sector

sector_agg0_to_agg3 <- function(data) { 
  
  data %>% 
    group_by(business_sector_str, business_sector_agg3) %>% 
    filter(row_number() == 1) %>% 
    select(business_sector_str, business_sector_agg3)
  
}

sample_sector <- function(ests, agg0_to_agg3) {
  
  ests %>%
    group_by(subgroup) %>%
    mutate(
      size_total = sum(total),
      share = total/size_total
    ) %>% ungroup() %>%
    mutate(
      valuelabel = paste0(pctclean(share, 1), "%")
    ) %>%
    filter(!is.na(group_cat_val) | group_cat_val != "Don't know") %>% 
    left_join(agg0_to_agg3, by = join_by(group_cat_val == business_sector_str)) %>% 
    mutate(
      group_cat_val = fct_reorder(group_cat_val, share)
    )
  
}

sample_sector_bycity <- function(ests, agg0_to_agg3) {
  
  ests %>%
    group_by(city, subgroup) %>%
    mutate(
      size_total = sum(total),
      share = total/size_total
    ) %>% ungroup() %>%
    mutate(
      valuelabel = paste0(pctclean(share, 1), "%")
    ) %>%
    filter(!is.na(group_cat_val) | group_cat_val != "Don't know") %>% 
    left_join(agg0_to_agg3, by = join_by(group_cat_val == business_sector_str)) %>% 
    mutate(
      group_cat_val = fct_reorder(group_cat_val, share)
    )
  
}

########################################################################
# 2. Small firm owner ------------------------------------
########################################################################


# Correlation plot between owner-level characteristics

corr_data <- function(corr) {
  
  levels <- c("Desire to have own business", "Lack of other income opportunities or desire to generate additional income", "Inherited business or other reason", "Grow the business", "Cover business costs", "Leave business to get a regular wage paying job or other", "Aggresively pursue risky opportunities that could generate significant additional income", "Cautiously pursue risk opportunities that could generate a little income", "Avoid risks even if they could generate additional income")
  
  flattenCorrMatrix(corr$r, corr$P) %>%
    mutate(
      row_name = INDICATORS[row],
      col_name = INDICATORS[column]
    ) %>% 
    separate_wider_delim(row_name, delim = ": ", names = c("row_group", "row_name"), too_few = "align_start") %>% 
    separate_wider_delim(col_name, delim = ": ", names = c("col_group", "col_name"), too_few = "align_start") %>%
    mutate(
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
      row_group = fct_inorder(row_group)
    ) %>%
    filter(drop != 1) %>%
    filter(row_group != "Risk tolerance")
  
}

corr_chart <- function(data) {
  
  ggplot(data = data,
         aes(x = col_name,
             y = fct_rev(row_name),
             fill = cor)
  ) +
    facet_grid(rows = vars(row_group), cols = vars(col_group), scales = "free", switch = "y") +
    geom_tile(aes(alpha = alpha)) +
    geom_text(aes(label = valuelabel)) +
    scale_x_discrete(position = "top") +
    scale_alpha_identity() +
    scale_fill_distiller(palette = "RdBu", direction = 1) +
    theme_custom() +
    labs(
      y = NULL,
      x = NULL,
      title = "The relationship between MSE owner's motivations, goals and attitudes to risk",
      subtitle = 'Correlation coefficient',
      caption = str_wrap(paste(SOURCE, 'Notes: Correlation coefficients significant with a p-value of 0.05 or less are labeled with an *.'), 170)
    ) +
    theme(strip.text.y = element_text(face = "bold"), legend.position = "none")
  
  
}




########################################################################
# 3. Digital technology section ------------------------------------
########################################################################

tech_adoption_landscape <- function(ests, wrap) {
  
  cats <- c("Lo", "Mlo", "Mhi", "Hi")
  blues <- brewer.pal(4, "Blues")
  names(blues) <- cats
  reds <- brewer.pal(4, "Reds")
  names(reds) <- cats

  ests %>%
    filter(!is.na(indicator_name)) %>% 
    mutate(
      valuelabel = ifelse(indicator_group == "Digital technology adoption score", numclean(mean,1), paste0(round(mean*100, 0), "%")), 
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
      fillcolor = ifelse(indicator_name %in% str_wrap(c("Never used", "Stopped using"), wrap), reds[valuecat], blues[valuecat]), 
      fillcolor = ifelse(indicator_group == "Digital technology adoption score", blues[valuecat2], fillcolor)
    ) 
  
}


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

########################################################################
# 5. Risks & Resilience section ------------------------------------
########################################################################

resilience_capital <- function(ests) {
  
  blues <- brewer.pal(4, "Blues")
  reds <- brewer.pal(4, "Reds")
  palette <- c(rev(reds), blues)
  names(palette) <- seq(1, 8, 1)
  
  ests %>%
    filter(!is.na(indicator_name)) %>% 
    mutate(
      indicator_type = ifelse(indicator %in% c("resi_capital_score_v1", "resi_capital_score_v2", "resi_capital_score_v3"), "cont", "disc"), 
      valuelabel = ifelse(indicator %in% c("resi_capital_score_v1", "resi_capital_score_v2", "resi_capital_score_v3"), numclean(mean,1), paste0(round(mean*100, 0), "%")), 
      valuecat = ifelse(indicator %not_in% c("resi_capital_score_v1", "resi_capital_score_v2", "resi_capital_score_v3"), cut(mean, breaks = seq(1,9,1)/8, labels = seq(1,8,1)), NA), 
      valuecat = ifelse(indicator %in% c("resi_capital_score_v1"), cut(mean, breaks = (seq(1,9,1)/8)*4, labels = seq(1,8,1)), valuecat), 
      valuecat = ifelse(indicator %in% c("resi_capital_score_v2"), cut(mean, breaks = (seq(1,9,1)/8)*3, labels = seq(1,8,1)), valuecat), 
      valuecat = ifelse(indicator %in% c("resi_capital_score_v3"), cut(mean, breaks = (seq(1,9,1)/8)*7, labels = seq(1,8,1)), valuecat), 
      fillcolor = palette[valuecat] 
    ) %>% 
    separate(indicator_name, into = c("indicator_group2", "indicator_name"), sep = ",", )
  
}



########################################################################

# Conducting analysis for Part 1 of report --------------------------

# 1.	The business landscape, how many firms did we discover and what do they do?
# a.	What is our estimate of the total number of businesses by size and sector in each of the study areas?
# b.	How do small firms that are the focus of our study (<= 10) differ in terms of their industrial sector from larger businesses (>10)?
# c.	Among the target population of firms (small firms (<=10)), what is the composition by size, sector, age of firm and sales?
# d.	Geospatial distribution with size of clusters

#####################################################################################

# Calculating total number of businesses
# Approach 1:
indicators <- c("N_business_total_percluster")
groups <- GROUPS["fullsample"]

# Total number of businesses found per cluster
compute_summary_clusterlevel_1g(indicators, groups, businesses_percluster %>% filter(N_business_total_percluster > 0), weights_cluster, keep = "total")

# Approach 2:
indicators <- c("business_total")
groups <- GROUPS[c("fullsample", "business_size_agg2", "business_sector_agg3")]

# Total number of businesses found
compute_summary_clusterlevel_1g(indicators, groups, enum_data, weights_cluster, keep = "total")

# Figure 1 Total number of firms by size and sector -----------------------

fig_1_data <- function(ests) {

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

fig_1_title <- function(ests, fig_data) {

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

  title <- glue('An estimated {global_total} (+/- {margin}%) businesses operate in the {CITY} study area. Of these {pct_small}% have 10 or fewer regular employees')
  return(title)

  }

# Figure 2 Share of MSEs by size and sector ----------------------- TAKEN OUT

fig_2_data <- function(ests) {

  ests %>%
    ungroup() %>%
    group_by(business_size_agg2) %>%
    mutate(
      size_total = sum(total),
      share = total/size_total,
      valuelabel = paste0(pctclean(share, 0), "%"),
      share_total = sum(share),
      barlabel = ifelse(group_cat_val == "Services: transportation, construction, repair, other", pctclean(share_total, 0), NA),
      barlabelpos = ifelse(group_cat_val == "Services: transportation, construction, repair, other", share_total, NA)
    ) %>%
    ungroup() %>%
    filter(group_cat_val != "Don't know")

}

# Figure 2 ALTERNATIVE ON FIRM PERFORMANCE -----------------------

fig_2a_data <- function(ests, indicators, groups) {

  ests %>%
    mutate(
      group_name = fct_rev(factor(group_name, levels = groups, ordered = TRUE)),
      indicator_name = factor(indicator_name, levels = indicators, ordered = TRUE),
      valuelabel = paste0(numclean(mean, 2)),
      currency_type = ifelse(str_detect(indicator_name, "LCU"), "Local currency", "USD"),
      indicator_group = "Revenues & Productivity"
    ) %>%
    filter(!is.na(group_cat_val)) %>%
    filter(group_cat_val != "Don't know")

}

# Figure 3  -----------------------

fig_3_data <- function(ests, groups) {

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
      valuelabel = paste0(str_wrap(group_cat_val, 20), "\n", pctclean(share, 0), "%"),
    ) %>%
    filter(!is.na(group_cat_val))

}

# Figure 4  -----------------------

fig_4_data <- function(ests) {

  ests %>%
    mutate(
      valuelabel = paste0(pctclean(mean, 0), "%"),
      group_cat_val = factor(group_cat_val, levels = GROUP_CAT_LEVELS, ordered = TRUE),
      indicator_group = ""
    ) %>%
    filter(!is.na(group_cat_val))

}





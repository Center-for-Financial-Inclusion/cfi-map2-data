
# ++++++++++++++++++++++++++++++++++++++++++++
# Helper functions -------------------------
# ++++++++++++++++++++++++++++++++++++++++++++

# Creates a codebook mapping variable names to labels
gen_varbook_vec <- function(data) {
  # Creating a named vector with all variable names and labels
  n <- ncol(data)
  labels <- as.character(map(seq(1,n), function(x) attr(data[[x]], "label")))
  names(labels) <- names(data)

  return(labels)
}

varbook_to_df <- function(varbook) {

  tibble(
    variable_name = names(varbook),
    variable_label = varbook
  )

  }

gen_valuebook <- function(data) {

  vars <- names(data)
  valuelabels <- map(vars, function(x) attr(data[[x]], "labels"))
  names(valuelabels) <- vars

  return(valuelabels)

  }


# Get attributes
get_attr <- function(x, name) {
  attributes(x)[[name]]
}

`%not_in%` <- negate(`%in%`)

pctclean <- function(x, n = 1) {
  if (max(x, na.rm = TRUE) <= 1) {
    round(x*100,n)
  } else {
    round(x, n)
  }
}

numclean <- function(x, n = 0) {
  prettyNum(round(x, n), big.mark = ",")
}

hyphen <- function(x) {
  paste(x, collapse = "-")
}

num_and_pct <- function(x, y) {

   z <- pctclean(x/y, 0)
   x <- numclean(x)

   paste0(x, " (", z, "%)")

}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

invert <- function(x) {
  x <- ifelse(x == 1, 0, 1)
}

roundupN <- function(x, n = 5) {
  (ceil(x*100/n, 1)*n)/100
} 

# Factor functions 

wrap_factor <- function(x, size = NULL) { 
  
  if (is.null(size)) { 
    fct_inorder(x) 
    } else { 
    fct_inorder(str_wrap(x, size)) 
    }

}

reverse_factor <- function(x, reverse_order = NULL) { 
  
  if (reverse_order) { 
    fct_rev(x)
  } else { 
    x  
    }

}

reorder_factor <- function(x, order_var = NULL) { 
  
  if (!is.null(order_var)) { 
    order_var <- sym(order_var)
    fct_reorder(x, !!order_var)
  } else { 
    x
    }

  }

style_factors <- function(x, size, reverse_order, order_var) { 
  
     x <- wrap_factor(x, size)
     x <- reverse_factor(x, reverse_order)   
     x <- reorder_factor(x, order_var)

  return(x)
  
  }
  
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# ++++++++++++++++++++++++++++++++++++++++++++
# GGPLOT Figure theme ---------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++

theme_custom <- function(scale_f = 1, ...) {
  theme_gray() +
    theme(
      plot.margin = margin(10, 5, 5, 5, "pt"),
      plot.subtitle = element_text(size = 11.5*scale_f, face = 'plain'),
      plot.title = element_text(size = 14.25*scale_f, hjust = 0, face = 'bold', color = 'black'),
      axis.text.x = element_text(hjust = 0.5),
      axis.text.y = element_text(hjust = 1),
      axis.title.y = element_text(color="black", size = 10*scale_f, angle = 90, vjust = 1, hjust = 1),
      axis.title.x = element_text(color="black", hjust = 0.5, size=10*scale_f),
      axis.text = element_text(size=10*scale_f, color = "black"),
      #panel.grid = element_line(color = "#F5E3E0", size = 0.25),
      plot.caption=element_text(hjust=0, color="gray30", size=10.5*scale_f),
      panel.background = element_rect(fill = "#ffffff"),
      rect = element_rect(fill = "#ffffff", colour = "#ffffff", size = 0.5, linetype = 1),
      axis.line.x  = element_blank(), 
      axis.line.y  = element_blank(), 
      strip.background = element_rect(fill = "#ffffff"),
      strip.text = element_text(size = 10*scale_f, face = 'bold'),
      strip.text.y = element_text(angle = 90),
      strip.text.y.left = element_text(hjust = 1, angle = 0),
      strip.placement = "outside",
      legend.position = "bottom", 
      legend.title = element_text(size = 10*scale_f),
      legend.text = element_text(size = 10*scale_f),
      legend.key.height = unit(1, 'lines'),
      legend.key.width = unit(1, 'lines'),
      legend.background = element_rect(fill = NA, colour = NA),
      legend.key = element_rect(colour = "transparent", fill = "white"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      ...
    )
}

# ++++++++++++++++++++++++++++++++++++++++++++
# Functions to compute statistics from survey data --------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++

options(survey.lonely.psu="certainty")

svy_summary_1g <- function(data, i, g, psu, strata, w, iref, gref) {

  i <- sym(i)
  g <- sym(g)
  w <- sym(w)

  data <- data %>% filter(!is.na(!!g))
  
  if (!is.null(psu)) {
    psu <- sym(psu)
  }

  if (!is.null(strata)) {
    strata <- sym(strata)
  }

  # Defining survey design
  if (!is.null(psu) & !is.null(strata)) {
    svydesign <- data %>%
      as_survey_design(ids = !!psu, weight = !!w, strata = !!strata, variables = c(!!i, !!g), nest = TRUE)
  } else if (!is.null(psu) & is.null(strata)) {
    svydesign <- data %>%
      as_survey_design(ids = !!psu, weight = !!w, variables = c(!!i, !!g), nest = TRUE)
  }
  else {
    svydesign <- data %>%
      as_survey_design(weight = !!w, variables = c(!!i, !!g), nest = TRUE)
  }

  # Computing mean and 95% confidence interval, adding a name fot the indicator, and group namees for group variable

  if (as_string(g) %in% c("fullsample")) {
    results <- svydesign %>%
      summarise(
        total = survey_total(!!i, vartype = "ci", na.rm = TRUE),
        mean = survey_mean(!!i, vartype = "ci", na.rm=TRUE),
        median = survey_median(!!i, vartype = "ci", na.rm = TRUE)
      ) %>%
      mutate(
        group_cat_val = gref[as_string(g)]
      )
    n <- data %>% summarise(nobs = sum(!is.na(!!i)))
    results <- results %>% bind_cols(n)
  }

  else {
    results <- svydesign %>%
      group_by(!!g) %>%
      summarise(
        total = survey_total(!!i, vartype = "ci", na.rm = TRUE),
        mean = survey_mean(!!i, vartype = "ci", na.rm=TRUE),
        median = survey_median(!!i, vartype = "ci", na.rm = TRUE)
      ) %>%
      mutate(
        group_cat_val = !!g
      )
    n <- data %>% group_by(!!g) %>% summarise(nobs = sum(!is.na(!!i)))
    results <- results %>% inner_join(n, by = as_string(g))
  }

  results <- results %>%
    mutate(
      indicator = as_string(i),
      indicator_name = iref[as_string(i)],
      group = as_string(g),
      group_name = gref[as_string(g)]
    ) %>%
    dplyr::select(
      indicator, indicator_name, group, group_name, group_cat_val, nobs, total, total_low, total_upp, mean, mean_low, mean_upp, median, median_low, median_upp
    )

}

svy_summary_2g <- function(data, i, g_l1, g_l2, psu, strata, w, iref, gref) {

  i <- sym(i)
  g_l1 <- sym(g_l1)
  g_l2 <- sym(g_l2)
  w <- sym(w)

  if (!is.null(psu)) {
    psu <- sym(psu)
  }

  if (!is.null(strata)) {
    strata <- sym(strata)
  }

  # Defining survey design
  if (!is.null(psu) & !is.null(strata)) {
    svydesign <- data %>%
      as_survey_design(ids = !!psu, weight = !!w, strata = !!strata, variables = c(!!i, !!g_l1, !!g_l2), nest = TRUE)
  } else if (!is.null(psu) & is.null(strata)) {
    svydesign <- data %>%
      as_survey_design(ids = !!psu, weight = !!w, variables = c(!!i, !!g_l1, !!g_l2), nest = TRUE)
  }
  else {
    svydesign <- data %>%
      as_survey_design(weight = !!w, variables = c(!!i, !!g_l1, !!g_l2), nest = TRUE)
  }

  # Computing mean and 95% confidence interval, adding a name fot the indicator, and group namees for group variable

  if (as_string(g_l2) %in% c("fullsample")) {
    results <- svydesign %>%
      group_by(!!g_l1) %>%
      summarise(
        total = survey_total(!!i, vartype = "ci", na.rm = TRUE),
        mean = survey_mean(!!i, vartype = "ci", na.rm=TRUE),
        median = survey_median(!!i, vartype = "ci", na.rm = TRUE)
      ) %>%
      mutate(
        #group1_cat_val = !!g_l1,
        group_cat_val = gref[as_string(g_l2)],
      )
    n <- data %>% group_by(!!g_l1) %>% summarise(nobs = sum(!is.na(!!i)))
    results <- results %>% inner_join(n, by = as_string(g_l1))
  }

  else {
    results <- svydesign %>%
      group_by(!!g_l1, !!g_l2) %>%
      summarise(
        total = survey_total(!!i, vartype = "ci", na.rm = TRUE),
        mean = survey_mean(!!i, vartype = "ci", na.rm=TRUE),
        median = survey_median(!!i, vartype = "ci", na.rm = TRUE)
      ) %>%
      mutate(
        #group1_cat_val = !!g_l1,
        !!g_l2 := as.character(!!g_l2),
        group_cat_val = !!g_l2
      )
    n <- data %>% group_by(!!g_l1, !!g_l2) %>% summarise(nobs = sum(!is.na(!!i)))
    results <- results %>% inner_join(n, by = c(as_string(g_l1), as_string(g_l2)))
  }

  results <- results %>%
    mutate(
      indicator = as_string(i),
      indicator_name = iref[as_string(i)],
      group = as_string(g_l2),
      group_name = gref[as_string(g_l2)]
    ) %>%
    dplyr::select(
      !!g_l1, indicator, indicator_name, group, group_name, group_cat_val, nobs, total, total_low, total_upp, mean, mean_low, mean_upp, median, median_low, median_upp
    )

}



# Kmeans to create 3 clusters of businesses defined by mtoviation, goals and attitude to risk

dat <- main_data %>% select(resp_motivation_entrep, resp_motivation_lackopp, resp_motivation_oth,
                           resp_maingoal_grow, resp_maingoal_leave, resp_maingoal_stab,
                           resp_riskapproach_aggr, resp_riskapproach_avoid, resp_riskapproach_calc)

dat<- dat[complete.cases(dat), ]

# Kmeans clustering
set.seed(123)
km.res <- kmeans(dat, 3, nstart = 25)



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

res <- rcorr(as.matrix(dat))

levels <- c("Desire to have own business", "Lack of other income opportunities or desire to generate additional income", "Inherited business or other reason", "Grow the business", "Cover business costs", "Leave business to get a regular wage paying job or other", "Aggresively pursue risky opportunities that could generate significant additional income", "Cautiously pursue risk opportunities that could generate a little income", "Avoid risks even if they could generate additional income")

chart_df <- flattenCorrMatrix(res$r, res$P) %>%
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


ggplot(data = chart_df,
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
    title = "Correlation between motivations, goals and attitudes to risk"
  ) +
  theme(strip.text.y = element_text(face = "bold"), legend.position = "none")



model <- lm(resp_psych_segment_shc_grow ~ resp_sex_women + resp_experience + resp_education_agg4, data = main_data)


depvars <- c("resp_psych_segment_shc_grow", "resp_psych_segment_shc_stab", "resp_psych_segment_shc_surv")
depvar_labels <- c("Growth and risk oriented", "Stability oriented", "Survival oriented")
names(depvar_labels) <- depvars

results <- bind_rows(
  map(depvars, model_and_prepfig, maineffect = "resp_sex_women", confounds = NULL, data = main_data),
  map(depvars, model_and_prepfig, maineffect = "resp_sex_women", confounds = c("resp_experience_c", "resp_education_agg4_shc_non", "resp_education_agg4_shc_sec",  "resp_education_agg4_shc_trt"), data = main_data)
)


chart_df <-
  results %>%
  mutate(
    depvar_label = depvar_labels[depvar],
    depvar_label = factor(depvar_label, levels = depvar_labels, ordered = TRUE),
    model_type = factor(ifelse(confounds_flag == 0, "Model: Unadjusted", "Model: Adjusted"), levels = c("Model: Unadjusted", "Model: Adjusted"), ordered = TRUE)
  ) %>%
  group_by(depvar, confounds_flag) %>%
  mutate(max = max(fig_data),
         max = ifelse(label == "Women", max, NA),
         min = min(fig_data),
         min = ifelse(label == "Women", min, NA),
         maineffect = ifelse(term == "resp_sex_women", estimate, NA),
         valuelabel = round(max(ifelse(label == "Women", estimate, NA), na.rm = TRUE)*100, 1),
         valuelabel_dollar = prettyNum(round(max(ifelse(label == "Women", estimate, NA), na.rm = TRUE), 0), big.mark = ","),
         sig = ifelse(p.value < 0.1, ".", NA),
         sig = ifelse(p.value < 0.05, "*", sig),
         sig = ifelse(p.value < 0.01, "**", sig),
         sig = ifelse(p.value < 0.001, "***", sig),
         sig = ifelse(is.na(sig), "", sig),
         valuelabel = paste0(valuelabel, "pp", sig),
         valuelabel_pos = (max - min)/2 + min,
         barlabel = round(fig_data*100, 1),
         barlabel_dollar = prettyNum(round(fig_data, 0), big.mark = ","),
         annotation = paste("R-sqrd:", round(adj_rsquared, 3), sep = " "),
         annotation = ifelse(label == "Women", NA, annotation)
  )

###

ggplot(data = chart_df,
       aes(x = label,
           y = fig_data)
) +
  facet_grid(cols = vars(depvar_label), rows = vars(model_type), switch = "y") +
  geom_col(width = 0.5, fill = "grey80") +
  geom_segment(aes(y = max, yend = min), color = "red", arrow = arrow(length=unit(.2, 'cm'))) +
  geom_point(aes(y = max), shape = 21, color = "red", fill = "white") +
  geom_text(aes(label = barlabel), vjust = 1, nudge_y = -0.01, color = "black") +
  geom_text(aes(y = valuelabel_pos, label = valuelabel), color = "red", hjust = 0) +
  geom_text(aes(y = 0.95, label = annotation), hjust = 0, color = "black", nudge_x = 0, size = 3.5) +
  labs(
    title = "Observational estimates of the relationship of owner's gender on the orientation of firms",
    y = "Predicted probability (%)",
    x = "Gender",
    caption = str_wrap("Notes: The results shown are based on a linear regression model that measures the effect of health insurance
                       coverage on several outcomes of interest. The 'unadjusted' model does not include any additional controls,
                       while the 'adjusted' model includes controls for three variables that theoretically confound the relationship
                       between insurnace and the outcomes explored here: years of education, an asset index (to proxy income) and employment.
                       The regression parameters are used to compute predicted probabilities for adults whose main source of income is not
                       from employment and who have mean levels of educational attainment and wealth, with and without health insurance coverage.", 140)
  ) +
  scale_y_continuous(limits = c(0, 1), label = scales::label_percent()) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15),
    strip.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.placement = "outside",
    axis.line.x = element_line(color = "black"),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, size = 10.5, color = "grey60"),
    plot.background = element_rect(fill = "white", color = "white"))



model_and_prepfig("resp_psych_segment_shc_grow", "resp_sex_women", "resp_experience_c", main_data)


#######
# Relationship between digital technology
#######

depvars <- c("resp_psych_segment_shc_grow", "resp_psych_segment_shc_stab", "resp_psych_segment_shc_surv")
depvar_labels <- c("Growth and risk oriented", "Stability oriented", "Survival oriented")
names(depvar_labels) <- depvars


model_and_prepfig("resp_psych_segment_shc_grow", "tech_function_total", "resp_experience_c", main_data)

model <- lm(perf_revphrpemp ~ tech_cat_any1 + tech_cat_any2 + tech_cat_any3 + tech_cat_all4 + resp_experience_c + resp_education_agg4_shc_non + resp_education_agg4_shc_sec + resp_education_agg4_shc_trt, data = main_data)
model <- lm(perf_revphrpemp ~ tech_function_total + resp_experience_c + resp_education_agg4_shc_non + resp_education_agg4_shc_sec + resp_education_agg4_shc_trt, data = main_data)
model <- lm(perf_revphrpemp ~ tech_has_internet + tech_function_comms + tech_function_mkts + tech_function_ops + tech_function_efin + resp_experience_c + resp_education_agg4_shc_non + resp_education_agg4_shc_sec + resp_education_agg4_shc_trt, data = main_data)

model <- feols(perf_revphrpemp_usd ~ tech_function_total, cluster = ~ Initial_block_ID, data = main_data)

model <- feols(perf_revphrpemp ~ tech_has_internet, cluster = ~ Initial_block_ID, data = main_data)

model <- feols(perf_revphrpemp ~ tech_cat_any1 + tech_cat_any2 + tech_cat_any3 + tech_cat_all4 + resp_experience_c + resp_education_agg4_shc_non + resp_education_agg4_shc_sec + resp_education_agg4_shc_trt,  cluster = ~ Initial_block_ID, data = main_data)
model <- feols(perf_revphrpemp ~ tech_function_total + resp_experience_c + resp_education_agg4_shc_non + resp_education_agg4_shc_sec + resp_education_agg4_shc_trt, cluster = ~ Initial_block_ID, data = main_data)
model <- feols(perf_revphrpemp ~ tech_function_comms + tech_function_mkts + tech_function_ops + tech_function_efin + resp_experience_c + resp_education_agg4_shc_non + resp_education_agg4_shc_sec + resp_education_agg4_shc_trt, cluster = ~ Initial_block_ID, data = main_data)
model <- feols(perf_revphrpemp ~ tech_has_internet + resp_experience_c + resp_education_agg4 + business_sector_agg3, cluster = ~ Initial_block_ID, data = main_data)


results <- bind_rows(
  map(depvars, model_and_prepfig, maineffect = "tech_function_index", confounds = NULL, data = main_data),
  map(depvars, model_and_prepfig, maineffect = "tech_function_index", confounds = c("resp_experience_c", "resp_education_agg4_shc_non", "resp_education_agg4_shc_sec",  "resp_education_agg4_shc_trt"), data = main_data)
)


chart_df <-
  results %>%
  mutate(
    depvar_label = depvar_labels[depvar],
    depvar_label = factor(depvar_label, levels = depvar_labels, ordered = TRUE),
    model_type = factor(ifelse(confounds_flag == 0, "Model: Unadjusted", "Model: Adjusted"), levels = c("Model: Unadjusted", "Model: Adjusted"), ordered = TRUE)
  ) %>%
  group_by(depvar, confounds_flag) %>%
  mutate(max = max(fig_data),
         max = ifelse(label == "Women", max, NA),
         min = min(fig_data),
         min = ifelse(label == "Women", min, NA),
         maineffect = ifelse(term == "resp_sex_women", estimate, NA),
         valuelabel = round(max(ifelse(label == "Women", estimate, NA), na.rm = TRUE)*100, 1),
         valuelabel_dollar = prettyNum(round(max(ifelse(label == "Women", estimate, NA), na.rm = TRUE), 0), big.mark = ","),
         sig = ifelse(p.value < 0.1, ".", NA),
         sig = ifelse(p.value < 0.05, "*", sig),
         sig = ifelse(p.value < 0.01, "**", sig),
         sig = ifelse(p.value < 0.001, "***", sig),
         sig = ifelse(is.na(sig), "", sig),
         valuelabel = paste0(valuelabel, "pp", sig),
         valuelabel_pos = (max - min)/2 + min,
         barlabel = round(fig_data*100, 1),
         barlabel_dollar = prettyNum(round(fig_data, 0), big.mark = ","),
         annotation = paste("R-sqrd:", round(adj_rsquared, 3), sep = " "),
         annotation = ifelse(label == "Women", NA, annotation)
  )


model <- feols(perf_revphrpemp_usd ~ tech_function_comms + tech_function_mkts + tech_function_ops + tech_function_efin, cluster = ~ Initial_block_ID, data = main_data)
model <- feols(perf_revphrpemp_usd ~ tech_function_comms + tech_function_mkts + tech_function_ops + tech_function_efin + resp_experience_c + resp_education_agg4_shc_non + resp_education_agg4_shc_sec + resp_education_agg4_shc_trt, cluster = ~ Initial_block_ID, data = main_data)

terms <- capture_terms_clse(depvar = "perf_revphrpemp", maineffects = c("tech_function_comms", "tech_function_mkts","tech_function_ops","tech_function_efin"), confounds = NULL, data = main_data)
effect_labels <- c("(Intercept)" = "No digital solutions used", INDICATORS[c("tech_function_comms", "tech_function_mkts","tech_function_ops","tech_function_efin")])
prep_fig(terms, effect_labels)




# Outputting table of indicators


groups <- GROUPS[c("fullsample")]
indicators <- names(INDICATORS)

ests <- compute_summary_mainlevel_1g(indicators, groups, data = main_data, weights = "weight_msme", psu = NULL, keep = "mean")

write_csv(ests, glue("outputs/indicators_{COUNTRY}.csv"))

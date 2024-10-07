########################################################################
# Preparing main interview data --------------------------
########################################################################

# Retreiving main interview data
main_raw_data <- get_main_data(COUNTRY, enum_data)

# Creating a codebook with the names and descriptions of variables
main_codebook <- varbook_to_df(gen_varbook_vec(main_raw_data))

# Returns a list with the values and valuelabels of each variable in the dataset
main_valuebook <- gen_valuebook(main_raw_data)
#main_valuebook[["Q2"]]
#main_valuebook[["Q5"]]

main_data <- prep_main_data(main_raw_data, weight_msme, COUNTRY) %>% filter(!is.na(weight_msme))
# Adding mca (pyschology) dimensions to data
main_data <- add_pca_todata(main_data)

# Outputting table of weights developed by Igor
write_csv(main_data %>% group_by(Initial_block_ID, Cluster_number) %>% filter(row_number() == 1) %>% select(Initial_block_ID, Cluster_number, w), glue("outputs/weights_percluster_{COUNTRY}_Igor.csv"))



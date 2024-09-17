

# Outputting table of weights per cluster

weights_mci <- main_data %>% group_by(Initial_block_ID, Cluster_number) %>% filter(row_number() == 1) %>% select(Initial_block_ID, Cluster_number, w)

weights_cfi <- weights_percluster  %>% select(Initial_block_ID, block_category_str, N, n, m, I, B, p1, weight_cluster, p2, weight_msme) %>% left_join(weights_mci, by = join_by(Initial_block_ID))

write_csv(weights_cfi, glue("outputs/weights_percluster_{COUNTRY}.csv"))


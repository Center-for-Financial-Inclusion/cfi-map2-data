
########################################################################
# Computing weights --------------------------
########################################################################

# Initial sampled blocks
initial_sampled_blocks <- get_sampled_blocks(COUNTRY, WEIGHT_PARAMS[[COUNTRY]][["restricted_blocks"]])

# Enumeration data
enum_data <- prep_enumeration_data(COUNTRY)

# Creating a codebook with the names and descriptions of variables
enum_codebook <- gen_varbook_vec(enum_data)

# Blocks per cluster
blocks_percluster <- prep_blocks_percluster(enum_data)

# Number of businesses per cluster
businesses_percluster <- prep_businesses_percluster(enum_data, initial_sampled_blocks)

# Number of businesses per unique block
businesses_perblock <- prep_businesses_perblock(enum_data, initial_sampled_blocks)

# Calculating weights
weights_percluster <- prep_weights(businesses_percluster, blocks_percluster, COUNTRY, WEIGHT_PARAMS)

# DF with the cluster-level weights
weights_cluster <- weights_percluster %>% select(Initial_block_ID, weight_cluster)
weight_msme <- weights_percluster %>% select(Initial_block_ID, weight_msme)

# DF with msme-level weights assigned to BlockID
#weights_msme <- businesses_perblock %>% select(BlockID, Initial_block_ID) %>% right_join(weights_percluster %>% select(Initial_block_ID, weight_msme), by = join_by(Initial_block_ID))




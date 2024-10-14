
# WRAPPING WEIGHTING FUNCTIONS INTO A SINGLE FUNCTION

weights_package <- function(country, enum_data, weight_params, return_this = "msme") { 
  # Enumeration data
  initial_sampled_blocks <- get_sampled_blocks(country, weight_params[[country]][["restricted_blocks"]])
  
  # Blocks per cluster
  blocks_percluster <- prep_blocks_percluster(enum_data)
  
  # Number of businesses per cluster
  businesses_percluster <- prep_businesses_percluster(enum_data, initial_sampled_blocks)
  
  # Number of businesses per unique block
  businesses_perblock <- prep_businesses_perblock(enum_data, initial_sampled_blocks)
  
  # Calculating weights
  weights_percluster <- prep_weights(businesses_percluster, blocks_percluster, country, weight_params)
  
  weight_msme <- weights_percluster %>% select(Initial_block_ID, weight_msme)
  
  if (return_this == "msme") {
    return(weight_msme)
  } 
  
  if (return_this == "cluster") {
    return(weights_percluster)
  }
  
} 

compile_enumdata <- function(country, weight_params) {
  
  enum_data <- prep_enumeration_data(country)
  weight_clusters <- weights_package(country, enum_data, weight_params, return_this = "cluster")
  
  enum_data %>% 
    left_join(weight_clusters, by = c("Initial_block_ID")) %>%
    mutate(fullsample = "All businesses")
  
}

maindata_package <- function(country, enum_data, weight_msme) { 
  
  # Retreiving main interview data
  main_raw_data <- get_main_data(country, blockID_to_InitialID(enum_data))
  
  main_data <- prep_main_data(main_raw_data, weight_msme, country) %>% filter(!is.na(weight_msme))
  # Adding mca (pyschology) dimensions to data
  main_data <- add_pca_todata(main_data)
  
  return(main_data)
  
}

compile_maindata <- function(country, weight_params) { 
  
  enum_data <- prep_enumeration_data(country)
  weight_msme <- weights_package(country, enum_data, weight_params)
  main_data <- maindata_package(country, enum_data, weight_msme)
  
  return(main_data)
  
  }

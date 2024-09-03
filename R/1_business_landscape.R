
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






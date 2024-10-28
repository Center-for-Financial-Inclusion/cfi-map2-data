

enumerated <- enum_data %>% filter(business_size_agg2 == "1-10 people")
interviewed <- enum_data %>% filter(business_size_agg2 == "1-10 people" & business_interviewed == 1)

table(enumerated$business_sector_agg4)
table(interviewed$business_sector_agg4)

table(enumerated$business_premise)
table(interviewed$business_premise)

table(enumerated$business_sector_agg4)/sum(table(enumerated$business_sector_agg4))
table(interviewed$business_sector_agg4)/sum(table(interviewed$business_sector_agg4))


table(x$business_sector_agg3)/sum(table(x$business_sector_agg3))
table(main_data$business_sector_agg3)/sum(table(main_data$business_sector_agg3))
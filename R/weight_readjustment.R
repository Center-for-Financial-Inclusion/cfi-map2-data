
View(loans_data %>% filter(lender_type %in% c("Shopkeeper: Cash loans", "Shopkeeper: Goods on credit")))


x <- loans_data %>% filter(lender_type %in% c("Family, friend or neighbor", "Chama", "Shopkeeper: Cash loan"))
y <- loans_data_2019 %>% filter(lender_type %in% c("Family, friend or neighbor", "Chama", "Shopkeeper: Cash loan"))


mean(x$nloans_pastyear, na.rm = TRUE)



social_men_2021 <- loans_data %>% filter(lender %in% c("F", "I", "J") & resp_gender_fct == "Men") %>% mutate(nloans_pastyear > 100, 100, nloans_pastyear)
social_women_2021 <- loans_data %>% filter(lender %in% c("F", "I", "J") & resp_gender_fct == "Women") %>% mutate(nloans_pastyear > 100, 100, nloans_pastyear)

median(social_men_2021$nloans_pastyear, na.rm = TRUE)
median(social_women_2021$nloans_pastyear, na.rm = TRUE)



social_men_2019 <- loans_data_2019 %>% filter(lender %in% c("f", "i", "j") & resp_gender_fct == "Men") %>% mutate(nloans_pastyear > 100, 100, nloans_pastyear)
social_women_2019 <- loans_data_2019 %>% filter(lender %in% c("f", "i", "j") & resp_gender_fct == "Women") %>% mutate(nloans_pastyear > 100, 100, nloans_pastyear)

median(social_men_2019$nloans_pastyear, na.rm = TRUE)
median(social_women_2019$nloans_pastyear, na.rm = TRUE)

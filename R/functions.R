

# Functions to retrieve data ------------

get_sampling_grid <- function(country) {

  # Gets full list of initial sampled blocks for specified country

  filename <- glue("https://raw.githubusercontent.com/pgubb/cfi-map2-sampling-grids/main/outputs/{country}/stage1_blocks_final.csv")


  }

get_sampled_blocks <- function(country, restricted_blocks = NULL) {

  # Gets full list of initial sampled blocks for specified country

  filename <- glue("https://raw.githubusercontent.com/pgubb/cfi-map2-sampling-grids/main/outputs/{country}/stage1_blocks_final.csv")

  read_csv(filename) %>%
    filter(in_final_sample == 1) %>%
    select(Initial_block_ID = block_id, block_category_str) -> data

  if (!is.null(restricted_blocks)) {
    data %>%
    mutate(
      block_category_str = ifelse(Initial_block_ID %in% restricted_blocks, "Off-limits", block_category_str)
    ) -> data
  }

    return(data)

  }

get_interview_data <- function(country) {

  filename <- glue("data/{country}/MSE_MAIN_{country}.sav")

  read_sav(filename)

  }

get_fieldwork_summary_businesses <- function(data) {

 # Data = enumeration data
  data %>%
    summarize(
      business_total = sum(business_total),
      business_eligible= sum(business_eligible),
      business_selected = sum(business_selected),
      business_interviewed = sum(business_interviewed)
    )

}

# Functions to prepare data ----------

prep_enumeration_data <- function(country) {

  filename <- glue("data/{country}/TS_{country}.sav")

  data <- read_sav(filename)

  data <- data %>%
    mutate(
      fullsample = "All businesses",
      business_total = 1,
      business_eligible = ifelse(Eligibility == 1, 1, 0),
      business_selected = ifelse(Selected == 1, 1, 0),
      business_interviewed = ifelse(Completed == 1, 1, 0),
      Q1 = ifelse(Q1 == -1, 4, Q1),
      business_premise = names(attributes(Q1)$labels[Q1]),
      business_size_agg2 = names(attributes(Q3_1)$labels[Q3_1]),
      business_size_agg2 = ifelse(is.na(business_size_agg2), "Don't know", business_size_agg2),
      business_sector_str = names(attributes(Q2)$labels[Q2]),
      business_sector_agg2 = case_when(
        Q2 %in% seq(1,7,1) ~ "Manufacturing",
        Q2 %in% seq(8,22,1) ~ "Services, construction & other"
      ),
      business_sector_agg2 = ifelse(is.na(business_sector_agg2), "Don't know", business_sector_agg2),
      business_sector_agg3 = case_when(
        Q2 %in% seq(1,7,1) ~ "Manufacturing",
        Q2 %in% seq(8,13,1) ~ "Services: retail/wholesale trade (re-sale)",
        Q2 %in% seq(14,22,1) ~ "Services: transportation, construction, repair, other"
      ),
      business_sector_agg3 = ifelse(is.na(business_sector_agg3), "Don't know", business_sector_agg3)
    )

    if(country == "India") {
      data %>% rename(BlockID = Block_ID) -> data
      }

    return(data)

  }

# For india creating cross-link between BlockID and Initial_Block_ID
blockID_to_InitialID <- function(data) {

  data %>% group_by(BlockID) %>% filter(row_number()==1) %>% select(BlockID, Initial_block_ID)

}

prep_blocks_percluster <- function(data) {

  data %>%
    group_by(Initial_block_ID, BlockID, Type_of_block) %>%
    filter(row_number() == 1) %>%
    select(Initial_block_ID, BlockID, Type_of_block) %>%
    mutate(
      N_blocks_percluster = 1
    ) %>%
    group_by(Initial_block_ID) %>%
    summarize(
      N_blocks_percluster = sum(N_blocks_percluster)
    )

}

prep_businesses_percluster <- function(data, initial_blocks) {

  data %>%
    group_by(Initial_block_ID) %>%
    summarize(
      N_business_total_percluster = sum(business_total),
      N_business_eligible_percluster= sum(business_eligible),
      N_business_selected_percluster = sum(business_selected),
      N_business_interviwed_percluster = sum(business_interviewed)
    ) %>%
    ungroup() %>%
    right_join(initial_blocks, by = c("Initial_block_ID")) %>%
    filter(block_category_str != "Off-limits") %>%
    mutate(
      # For blocks that were not in the tracking sheet, these were visited but no businesses were found (i.e. residential etc)
      N_business_total_percluster = ifelse(is.na(N_business_total_percluster), 0, N_business_total_percluster),
      N_business_eligible_percluster = ifelse(is.na(N_business_eligible_percluster), 0, N_business_eligible_percluster),
      N_business_selected_percluster = ifelse(is.na(N_business_selected_percluster), 0, N_business_selected_percluster),
      N_business_interviwed_percluster =  ifelse(is.na(N_business_interviwed_percluster), 0, N_business_interviwed_percluster)
    )

}

prep_businesses_perblock <- function(data, initial_blocks) {

  data %>%
    group_by(BlockID, Initial_block_ID) %>%
    summarize(
      N_business_total_percluster = sum(business_total),
      N_business_eligible_percluster= sum(business_eligible),
      N_business_selected_percluster = sum(business_selected),
      N_business_interviwed_percluster = sum(business_interviewed)
    ) %>%
    ungroup() %>%
    right_join(initial_blocks, by = c("Initial_block_ID")) %>%
    filter(block_category_str != "Off-limits") %>%
    mutate(
      # For blocks that were not in the tracking sheet, these were visited but no businesses were found (i.e. residential etc)
      N_business_total_percluster = ifelse(is.na(N_business_total_percluster), 0, N_business_total_percluster),
      N_business_eligible_percluster = ifelse(is.na(N_business_eligible_percluster), 0, N_business_eligible_percluster),
      N_business_selected_percluster = ifelse(is.na(N_business_selected_percluster), 0, N_business_selected_percluster),
      N_business_interviwed_percluster =  ifelse(is.na(N_business_interviwed_percluster), 0, N_business_interviwed_percluster),
      BlockID = ifelse(is.na(BlockID), Initial_block_ID, BlockID)
    )

}

prep_weights <- function(business_data, blocks_data, country, weight_params) {

  if (country %in% c("India", "Brazil")) {
    n = length(unique(blocks_data$Initial_block_ID))
  } else {
    n = weight_params[[COUNTRY]][["n"]]
  }

  business_data %>%
    left_join(blocks_data, by = "Initial_block_ID") %>%
    # Mapping parameters for computing weights (per document) to variables
    mutate(
      # For blocks that were not in the tracking sheet, these were visited but no businesses were found (i.e. residential etc)
      N_blocks_percluster = ifelse(is.na(N_blocks_percluster), 1, N_blocks_percluster),
      N = weight_params[[COUNTRY]][["N"]],
      n = n,
      m = N_blocks_percluster,
      I = N_business_interviwed_percluster,
      B = N_business_eligible_percluster
    ) %>%
    # Computing "Stage 1" probability (probability cluster is included in sample)
    mutate(
      p1 = 1 - (choose(N - m, n)/choose(N, n)),
      weight_cluster = 1/p1,
      p2 = I/B,
      weight_msme = 1/(p1*p2)
    )

  }

# Functions to prepare MAIN interview data ----------

get_main_data <- function(country, blockID_to_InitialID = NULL) {

  filename <- glue("data/{country}/MSE_MAIN_{country}.sav")

  data <- read_sav(filename) %>% mutate(country = country)

  if (country %in% c("Nigeria")) {
    data %>%
      mutate(
        Initial_block_ID = ifelse(str_length(as.character(BlockID)) > 4, str_sub(as.character(BlockID), str_length(as.character(BlockID))-3), as.character(BlockID)),
        Initial_block_ID = as.numeric(Initial_block_ID)
      ) -> data
  } else if (country == "Ethiopia") {
    data %>%
      mutate(
        Initial_block_ID = BlockID
      ) -> data
  } else if (country == "India") {
      data %>%
        left_join(blockID_to_InitialID, by = c("BlockID")) -> data

    }

  return(data)

}

prep_main_data <- function(raw_data, weights, country) {

  if (country == "Nigeria") { 
    add_worker <- 1
  } else { 
    add_worker <- 0
    }
  
  main_raw_data %>%

    left_join(weights, by = join_by(Initial_block_ID)) %>%

    mutate(
      fullsample = "All businesses",
      business_total = 1,

      # Firm-level characteristics
      business_premise = names(attributes(Q1)$labels[Q1]),
      business_premise_shc = Q1,

      business_sector_str = names(attributes(Q2)$labels[Q2]),

      business_sector_agg2 = case_when(
        Q2 %in% seq(1,7,1) ~ "Manufacturing",
        Q2 %in% seq(8,22,1) ~ "Services"
      ),
      #business_sector_agg2 = ifelse(is.na(business_sector_agg2), "Don't know", business_sector_agg2),
      business_sector_agg2_shc = case_when(
        business_sector_agg2 == "Manufacturing" ~ "mfc",
        business_sector_agg2 == "Services" ~ "srv",
        business_sector_agg2 == "Don't know" ~ "dk"
      ),

      business_sector_agg3 = case_when(
        Q2 %in% seq(1,7,1) ~ "Manufacturing",
        Q2 %in% seq(8,13,1) ~ "Services: trade (re-sale)",
        Q2 %in% seq(14,22,1) ~ "Services: other (eg. transport, construction)"
      ),
      #business_sector_agg3 = ifelse(is.na(business_sector_agg3), "Don't know", business_sector_agg3),
      business_sector_agg3_shc = case_when(
        business_sector_agg3 == "Manufacturing" ~ "mfc",
        business_sector_agg3 == "Services: trade (re-sale)" ~ "srv_resale",
        business_sector_agg3 == "Services: other (eg. transport, construction)" ~ "srv_oth",
        business_sector_agg3 == "Don't know" ~ "dk"
      ),

      business_registered = ifelse(Q74 == 1, 1, 0),
      business_registered = ifelse(Q74 %in% c(97, 99), NA, business_registered),
      business_registration_status = names(attributes(Q74)$labels[Q74]),
      #business_sector_agg3 = factor(business_sector_agg4, levels = c("Services: retail & wholesale trade", "Services: other", "Manufacturing", "Construction & other"), ordered = TRUE),

      business_size = ifelse(Q6 == 0, 1, Q6) + add_worker,
      business_size_agg2 = ifelse(Q6 == 1, "1 person", "2-10 people"),
      business_size_agg2_shc = ifelse(business_size_agg2 == "1 person", "1", "2_10"),
      #business_size_agg2 = factor(business_size_agg2, levels = c("1 person", "2-10 people"), ordered = TRUE),

      # Respondent characteristics
      resp_owner = ifelse(Q4 %in% c(1,3), 1, 0), # Is the respondent the owner or a manager?
      resp_owner_str = case_when(
        Q4 %in% c(1, 3) ~ "Owner", 
        Q4 == 2 ~ "Manager", 
        Q4 == 4 ~ "Other"
      ), 
      
      resp_experience = ifelse(Q8 == 97, NA, Q8),
      resp_experience_c = resp_experience - mean(resp_experience, na.rm = TRUE),
      resp_experience_agg5 = case_when(
        Q8 <= 1 ~ '1 yr or less',
        Q8 <= 5 ~ '2-5 yrs',
        Q8 <= 10 ~ '6-10 yrs',
        Q8 <= 20 ~ '11-20 yrs',
        Q8 > 20 ~ '> 20 yrs'
      ),
      resp_experience_agg5 = ifelse(Q8 == 97, NA, resp_experience_agg5),
      #resp_experience_agg5 = factor(resp_experience_agg5, levels = c('1 year or less', '2-5 years', '6-10 years', '11-20 years', '> 20 years'), ordered = TRUE),
      resp_experience_agg5_shc = case_when(
        Q8 <= 1 ~ '1',
        Q8 <= 5 ~ '2_5',
        Q8 <= 10 ~ '6_10',
        Q8 <= 20 ~ '11_20',
        Q8 > 20 ~ '20'
      ),

      resp_age = ifelse(Q72 == 99, NA, Q72),
      resp_age_agg6 = case_when(
        resp_age < 25 ~ "[< 25 yrs]",
        resp_age >= 25 & resp_age < 35 ~ "[25-35 yrs)",
        resp_age >= 35 & resp_age < 45 ~ "[35-45 yrs)",
        resp_age >= 45 & resp_age < 55 ~ "[45-55 yrs)",
        resp_age >= 55 & resp_age < 65 ~ "[55-65 yrs)",
        resp_age >= 65 ~ "[65+ yrs]"
      ),
      resp_age_agg6_shc = case_when(
        resp_age < 25 ~ "25",
        resp_age >= 25 & resp_age < 35 ~ "2535",
        resp_age >= 35 & resp_age < 45 ~ "3545",
        resp_age >= 45 & resp_age < 55 ~ "4555",
        resp_age >= 55 & resp_age < 65 ~ "5565",
        resp_age >= 65 ~ "65"
      ),

      resp_education = ifelse(Q73 %in% c(97, 99), NA, Q73),
      resp_education_agg4 = case_when(
        resp_education <= 2 ~ "Some primary or none",
        resp_education <= 4 ~ "Completed primary or some secondary",
        resp_education %in% c(5, 6, 8, 10) ~ "Completed secondary or some university/technical school",
        resp_education %in% c(7, 9, 11) ~ "Completed university/technical school"
      ),
      resp_education_agg4_shc = case_when(
        resp_education <= 2 ~ "non",
        resp_education <= 4 ~ "pri",
        resp_education %in% c(5, 6, 8, 10) ~ "sec",
        resp_education %in% c(7, 9, 11) ~ "trt"
      ),

      resp_sex_str = ifelse(Q5 == 1, "Women", "Men"),
      resp_sex_men = ifelse(Q5 == 2, 1, 0),
      resp_sex_women = ifelse(Q5 == 1, 1, 0),

      resp_motivation = case_when(
        Q10 == 2 ~ "Desire to have own business",
        Q10 %in% c(1, 4) ~ "Lack of other income opportunities or desire to generate additional income",
        Q10 %in% c(3, 5) ~ "Inherited business or other reason"
      ),

      # Motivation: My primary goal is to be an entrepreneur, regardless of other income earning opportunities
      resp_motivation_entrep = ifelse(Q10 == 2, 1, 0),
      resp_motivation_entrep = ifelse(Q10 %in% c(97,99), NA, resp_motivation_entrep),

      # Motivation: Lack of other job opportunities or wanting to generate additional income
      resp_motivation_lackopp =  ifelse(Q10 %in% c(1, 4), 1, 0),
      resp_motivation_lackopp = ifelse(Q10 %in% c(97,99), NA, resp_motivation_lackopp),

      # Motivation: Other including inherited business
      resp_motivation_oth =  ifelse(Q10 %in% c(3,5), 1, 0),
      resp_motivation_oth = ifelse(Q10 %in% c(97,99), NA, resp_motivation_oth),

      resp_maingoal = case_when(
        Q11 == 2 ~ "Grow the business",
        Q11 == 1 ~ "Cover business costs",
        Q11 %in% c(3,4) ~ "Leave business to get a regular wage paying job or other"
      ),

      # Main goal: Growt a business
      resp_maingoal_grow = ifelse(Q11 == 2, 1, 0),
      resp_maingoal_grow = ifelse(Q11 %in% c(97,99), NA, resp_maingoal_grow),

      # Main goal: Cover business costs
      resp_maingoal_stab = ifelse(Q11 == 1, 1, 0),
      resp_maingoal_stab = ifelse(Q11 %in% c(97,99), NA, resp_maingoal_stab),

      # Main goal: Leave business (or other)
      resp_maingoal_leave = ifelse(Q11 %in% c(3, 4), 1, 0),
      resp_maingoal_leave = ifelse(Q11 %in% c(97,99), NA, resp_maingoal_leave),

      resp_riskapproach = case_when(
        Q12 == 3 ~ "Aggresively pursue risky opportunities that could generate significant additional income",
        Q12 == 2 ~ "Cautiously pursue risk opportunities that could generate a little income",
        Q12 == 1 ~ "Avoid risks even if they could generate additional income"
      ),

      # Approach to risk
      resp_riskapproach_aggr = ifelse(Q12 == 3, 1, 0),
      resp_riskapproach_aggr = ifelse(Q12 %in% c(5, 97), NA, resp_riskapproach_aggr),

      resp_riskapproach_calc = ifelse(Q12 == 2, 1, 0),
      resp_riskapproach_calc = ifelse(Q12 %in% c(5, 97), NA, resp_riskapproach_calc),

      resp_riskapproach_avoid =  ifelse(Q12 == 1, 1, 0),
      resp_riskapproach_avoid = ifelse(Q12 %in% c(5, 97), NA, resp_riskapproach_avoid),

      # Pyschogrpahic segment based on motivations, goals and approach to risk

      # Owner identifies as an entrepreneur, wants to grow by taking sizeable risks
      resp_psych_segment = ifelse(
              resp_motivation_entrep == 1 &
              resp_maingoal_grow == 1 &
              resp_riskapproach_aggr == 1, "Growth", NA),
      # Owner identifies as an entrepreneur, has
      resp_psych_segment = ifelse(
              resp_motivation_lackopp == 1 &
              resp_riskapproach_avoid == 1, "Survival", resp_psych_segment
        ),
      resp_psych_segment = ifelse(is.na(resp_psych_segment), "Stability", resp_psych_segment),

      resp_psych_segment_shc = case_when(
        resp_psych_segment == "Growth" ~ "grow",
        resp_psych_segment == "Stability" ~ "stab",
        resp_psych_segment == "Survival" ~ "surv"
      ),

      resp_psych_segment_agg2 = case_when(
        resp_psych_segment == "Growth" ~ "Growth",
        resp_psych_segment %in% c("Stability", "Survival") ~ "Stability or survival"
      ),

      # Percentage of total household income from business
      resp_pcthhincfrmbus = names(attributes(Q77)$labels[Q77]),
      resp_pcthhincfrmbus_high = ifelse(Q77 %in% c(3,4), 1, 0),
      resp_pcthhincfrmbus_high = ifelse(Q77 %in% c(97,99), NA, resp_pcthhincfrmbus_high),

      # Business has formal account
      business_account_formal = ifelse(Q30 == 1, 1, 0),
      business_account_formal = ifelse(Q40 %in% c(97,99), NA, business_account_formal),

      # Owner's account separate from business account
      business_account_separate = case_match(Q31, 1 ~ 1, 2 ~ 0, 99 ~ NA),

      # Business has no formal account or business account is not sperate from owner's account
      business_finances_notseparate = ifelse(business_account_formal == 0 | business_account_separate == 0, 1, 0),

      ###### DIGITAL TECHNOLOGY

      # Internet connectivity and devicue usage
      tech_has_internet = case_match(Q14, 1 ~ 1, 2 ~ 0, c(97,99) ~ NA),
      tech_has_device = ifelse(T_Q13_2 == 1 | T_Q13_3 == 1 | T_Q13_4 == 1, 1, 0), # Mobile phone, tablet, smartphone, laptop
      tech_has_both = ifelse(tech_has_internet == 1 & tech_has_device == 1, 1, 0),
      tech_has_none = ifelse(tech_has_internet == 0 & tech_has_device == 0, 1, 0),

      # Messaging apps
      tech_uses_messaging =  case_match(Q15, 1 ~ 1, 2 ~ 0, c(97,99) ~ NA),
      tech_uses_messaging_30da = case_match(Q16, c(1, 2, 3) ~ 1, c(4,5) ~ 0, c(97,99) ~ NA),
      tech_uses_messaging_7da = case_match(Q16, c(1, 2) ~ 1, c(3, 4,5) ~ 0, c(97,99) ~ NA),
      
      tech_uses_messaging_shc = case_when(
        tech_uses_messaging == 0 ~ "nvr", 
        Q16 == 5 ~ "nam", 
        Q16 %in% c(3, 4) ~ "mol", 
        Q16 %in% c(1, 2) ~ "dow"
      ), 
      
      #Social media
      tech_uses_socialmedia =  case_match(Q17, 1 ~ 1, 2 ~ 0, c(97,99) ~ NA),
      tech_uses_socialmedia_30da = case_match(Q18, c(1, 2, 3) ~ 1, c(4,5) ~ 0, c(97,99) ~ NA),
      tech_uses_socialmedia_7da = case_match(Q18, c(1, 2) ~ 1, c(3, 4,5) ~ 0, c(97,99) ~ NA),
      
      tech_uses_socialmedia_shc = case_when(
        tech_uses_socialmedia == 0 ~ "nvr", 
        Q18 == 5 ~ "nam", 
        Q18 %in% c(3, 4) ~ "mol", 
        Q18 %in% c(1, 2) ~ "dow"
      ), 
      
      # Website
      tech_uses_website =  case_match(Q19, 1 ~ 1, 2 ~ 0, c(97,99) ~ NA),
      tech_uses_website_imp =  case_match(Q20, c(1,2) ~ 1, c(3,4) ~ 0, c(97,99) ~ NA),
      
      # E-commerce platforms
      tech_uses_ecommerce =  case_match(Q21, 1 ~ 1, 2 ~ 0, c(97,99) ~ NA),
      tech_uses_ecommerce_30da = case_match(Q22, c(1, 2, 3) ~ 1, c(4,5) ~ 0, c(97,99) ~ NA),
      tech_uses_ecommerce_7da = case_match(Q22, c(1, 2) ~ 1, c(3,4,5) ~ 0, c(97,99) ~ NA),
      
      tech_uses_ecommerce_shc = case_when(
        tech_uses_ecommerce == 0 ~ "nvr", 
        Q22 == 5 ~ "nam", 
        Q22 %in% c(3, 4) ~ "mol", 
        Q22 %in% c(1, 2) ~ "dow"
      ), 
      
      # Software
      tech_uses_software = case_match(Q23,  1 ~ 1, 2 ~ 0, c(97,99) ~ NA),
      tech_uses_software_30da = case_match(Q24, c(1, 2, 3) ~ 1, c(4,5) ~ 0, c(97,99) ~ NA),
      tech_uses_software_7da = case_match(Q24, c(1, 2) ~ 1, c(3, 4,5) ~ 0, c(97,99) ~ NA),
      
      tech_uses_software_shc = case_when(
        tech_uses_software == 0 ~ "nvr", 
        Q24 == 5 ~ "nam", 
        Q24 %in% c(3, 4) ~ "mol", 
        Q24 %in% c(1, 2) ~ "dow"
      ), 
      
      tech_uses_ai = case_match(Q26,  1 ~ 1, 2 ~ 0, c(97,99) ~ NA),

      # Enterprise finance
      tech_uses_digpayments = ifelse(Q35_3 == 1 | Q35_4 == 1 | Q35_5 == 1 | Q35_6 == 1 | Q35_7 == 1 | Q35_8 == 1, 1, 0),

      tech_uses_digloans = ifelse(Q38_1 == 1 | Q38_2 == 1 | Q38_3 == 1 | Q38_4 == 1 | Q38_5 == 1 | Q38_6 == 1 |  Q37_3 == 1 | Q37_6 == 1 | Q37_7 == 1, 1, 0),
      tech_uses_digloans = ifelse(is.na(tech_uses_digloans), 0, tech_uses_digloans),

      tech_uses_diginsurance = ifelse(B2_1 == 1 | B2_2 == 1 | B2_3 == 1 | B2_4 == 1 | B2_5 == 1 | B2_6 == 1 | B2_7 == 1 | B2_8 == 1 | B2_9 == 1 | B2_10 == 1, 1, 0),
      tech_uses_diginsurance = ifelse(is.na(tech_uses_diginsurance), 0, tech_uses_diginsurance),
  
      tech_uses_adoption_score = tech_has_internet + tech_has_device + tech_uses_website + tech_uses_messaging + tech_uses_socialmedia + tech_uses_ecommerce + tech_uses_software + tech_uses_ai + tech_uses_digpayments + tech_uses_digloans,
      tech_uses_adoption_score_c = tech_uses_adoption_score - mean(tech_uses_adoption_score, na.rm = TRUE),
      
      # Functional perspective --------------
      
      # Communications
      # 30-day active
      tech_function_comms = ifelse(tech_uses_messaging == 1 | tech_uses_socialmedia == 1, 1, 0),
      tech_function_comms_30da= ifelse(tech_uses_messaging_30da == 1 | tech_uses_socialmedia_30da == 1, 1, 0),
      tech_function_comms_30da= ifelse(is.na(tech_function_comms_30da), 0, tech_function_comms_30da),
      # 7-day active
      tech_function_comms_7da= ifelse(tech_uses_messaging_7da == 1 | tech_uses_socialmedia_7da == 1, 1, 0),
      tech_function_comms_7da= ifelse(is.na(tech_function_comms_7da), 0, tech_function_comms_7da),
      
      # Access to markets
      # 30-day active
      tech_function_mkts = ifelse(tech_uses_ecommerce == 1 | tech_uses_website == 1, 1, 0),
      tech_function_mkts_30da =  ifelse(tech_uses_ecommerce_30da == 1 | tech_uses_website_imp == 1, 1, 0),
      tech_function_mkts_30da= ifelse(is.na(tech_function_mkts_30da), 0, tech_function_mkts_30da),
      # 7-day active
      tech_function_mkts_7da =  ifelse(tech_uses_ecommerce_7da == 1 | tech_uses_website_imp == 1, 1, 0),
      tech_function_mkts_7da= ifelse(is.na(tech_function_mkts_7da), 0, tech_function_mkts_7da),
      
      # Operations
      # 30-day active
      tech_function_ops = ifelse(tech_uses_software == 1 | tech_uses_ai == 1, 1, 0),
      tech_function_ops_30da =  ifelse(tech_uses_software_30da == 1, 1, 0),
      tech_function_ops_30da= ifelse(is.na(tech_function_ops_30da), 0, tech_function_ops_30da),
      # 7-day active
      tech_function_ops_7da =  ifelse(tech_uses_software_7da == 1, 1, 0),
      tech_function_ops_7da= ifelse(is.na(tech_function_ops_7da), 0, tech_function_ops_7da),
      
      # Enterprise digital finance  
      tech_function_epay = tech_uses_digpayments,
      tech_function_efin = ifelse(tech_uses_digloans == 1 | tech_uses_diginsurance == 1, 1, 0),

      tech_function_total = tech_function_comms + tech_function_mkts + tech_function_ops + tech_function_efin,
      tech_function_30da_total = tech_function_comms_30da + tech_function_mkts_30da + tech_function_ops_30da,

      tech_cat_none = ifelse(tech_function_comms == 0 & tech_function_mkts == 0 & tech_function_ops == 0 & tech_function_epay == 0, 1, 0),
      tech_cat_any1 = ifelse(tech_function_total == 1, 1, 0),
      tech_cat_any2 = ifelse(tech_function_total == 2, 1, 0),
      tech_cat_any3 = ifelse(tech_function_total == 3, 1, 0),
      tech_cat_all4 = ifelse(tech_function_total == 4, 1, 0),

      tech_function_index = (tech_function_total + tech_function_30da_total)/7,

      # Business performance ----------
      fx = unlist(FX_RATES[country]),
      perf_mpy = Q75, # Months per year of operations
      perf_mpy = ifelse(Q75 %in% c(97, 99), NA, perf_mpy),
      perf_hrspw = Q76, # hourse per week of operation
      perf_hrspw = ifelse(Q76 %in% c(97, 99), NA, perf_hrspw),
      perf_rev = as.numeric(zap_labels(main_raw_data$Q78)),
      perf_rev = ifelse(Q78 %in% c(97,99), NA, perf_rev),
      perf_rev_usd = perf_rev/fx,
      perf_hrspy = perf_hrspw*4*perf_mpy, #Total hours per year of operation
      perf_hrspy = perf_hrspy/12, #Avg. Hours per month of operation
      perf_revphr = perf_rev/perf_hrspy,
      perf_revphrpemp = perf_revphr/business_size, # Revenue per hour per employee
      perf_revphrpemp_usd = perf_revphrpemp/fx, # Revenue per hour per employee

      # Risks -------- 
      
      # In the last 36 months (3 years), which of the following risks had the largest impact (most costly) in terms of losses or expenses incurred by the business
      risk_largestimpact_str = names(attributes(Q56)$labels[Q56]),
      risk_largestimpact_str = ifelse(Q56 == 99, NA, risk_largestimpact_str), 
      
      risk_largestimpact_shc = case_when(
        Q56 == 1 ~ "wthr", 
        Q56 == 2 ~ "hlth", 
        Q56 == 3 ~ "corr", 
        Q56 == 4 ~ "crme", 
        Q56 == 5 ~ "frau", 
        Q56 == 6 ~ "cost", 
        Q56 == 7 ~ "othr",
        Q56 == 8 ~ "none"
      ), 
      
      risk_weather_type_heat = ifelse(Q57_1 == 1, 1, ifelse(Q57_1 == 2, 0, NA)),
      risk_weather_type_droughts = ifelse(Q57_2 == 1, 1, ifelse(Q57_2 == 2, 0, NA)),
      risk_weather_type_floods = ifelse(Q57_3 == 1, 1, ifelse(Q57_3 == 2, 0, NA)),
      risk_weather_type_typhoon = ifelse(Q57_4 == 1, 1, ifelse(Q57_4 == 2, 0, NA)),
      risk_weather_type_typhoon = ifelse(is.na(risk_weather_type_typhoon), 0, risk_weather_type_typhoon),
      risk_weather_type_floodstyphoon = ifelse(risk_weather_type_floods == 1 | risk_weather_type_typhoon == 1, 1, 0), 
      risk_weather_type_other = ifelse(Q57_5 == 1, 1, ifelse(Q57_5 == 2, 0, NA)),
      risk_weather_type_any = ifelse(risk_weather_type_heat == 1 | risk_weather_type_droughts == 1 | risk_weather_type_floodstyphoon == 1 | risk_weather_type_other == 1, 1, 0), 
      risk_weather_type_any_str = ifelse(risk_weather_type_any== 1, "Impacted by climate shock in past 36 months", "Not impacted by climate shock"), 
      
      risk_weather_impact_fin =  ifelse(Q58_1 == 1, 1, ifelse(Q58_1 == 2, 0, NA)),
      risk_weather_impact_cst = ifelse(Q58_2 == 1, 1, ifelse(Q58_2 == 2, 0, NA)),
      risk_weather_impact_ops = ifelse(Q58_3 == 1, 1, ifelse(Q58_3 == 2, 0, NA)),
      risk_weather_impact_dmg = ifelse(Q58_4 == 1, 1, ifelse(Q58_4 == 2, 0, NA)),
      risk_weather_impact_imp = ifelse(Q58_5 == 1, 1, ifelse(Q58_5 == 2, 0, NA)),
      
      risk_weather_cope_prod = ifelse(Q59_1 == 1, 1, ifelse(Q59_1 == 2, 0, NA)),
      risk_weather_cope_spch = ifelse(Q59_2 == 1, 1, ifelse(Q59_2 == 2, 0, NA)),
      risk_weather_cope_relo = ifelse(Q59_3 == 1, 1, ifelse(Q59_3 == 2, 0, NA)),
      risk_weather_cope_cash = ifelse(Q59_4 == 1, 1, ifelse(Q59_4 == 2, 0, NA)),
      risk_weather_cope_loan = ifelse(Q59_5 == 1, 1, ifelse(Q59_5 == 2, 0, NA)),
      risk_weather_cope_insu = ifelse(Q59_6 == 1, 1, ifelse(Q59_6 == 2, 0, NA)),
      risk_weather_cope_govr = ifelse(Q59_7 == 1, 1, ifelse(Q59_7 == 2, 0, NA)),
      risk_weather_cope_hcsh = ifelse(Q59_8 == 1, 1, ifelse(Q59_8 == 2, 0, NA)),
      risk_weather_cope_remi = ifelse(Q59_9 == 1, 1, ifelse(Q59_9 == 2, 0, NA)),
      risk_weather_cope_asst = ifelse(Q59_10 == 1, 1, ifelse(Q59_10 == 2, 0, NA)),
      risk_weather_cope_noth = ifelse(Q59_11 == 1, 1, ifelse(Q59_11 == 2, 0, NA)),
      risk_weather_cope_othr = ifelse(Q59_12 == 1, 1, ifelse(Q59_12 == 2, 0, NA)),
      
      risk_weather_cond_ntrc = ifelse(Q60 == 1, 1, 0), 
      risk_weather_cond_ntrc = ifelse(Q60 %in% c(97,99), NA, risk_weather_cond_ntrc), 
      risk_weather_cond_rc = ifelse(Q60 == 2, 1, 0), 
      risk_weather_cond_rc = ifelse(Q60 %in% c(97,99), NA, risk_weather_cond_rc), 
      risk_weather_cond_rcbt = ifelse(Q60 == 3, 1, 0), 
      risk_weather_cond_rcbt = ifelse(Q60 %in% c(97,99), NA, risk_weather_cond_rcbt), 
      
      risk_weather_adaptspend = ifelse(Q61 == 1, 1, 0), 
      risk_weather_adaptspend = ifelse(Q61 %in% c(97,99), NA, risk_weather_adaptspend), 
      
      # Resilience ---------------------
      resi_efunds_firm_diff_3 = ifelse(Q62 == 1, 1, ifelse(Q62 %in% c(97,99), NA, 0)), 
      resi_efunds_firm_diff_2 = ifelse(Q62 == 2, 1, ifelse(Q62 %in% c(97,99), NA, 0)),
      resi_efunds_firm_diff_1 = ifelse(Q62 == 3, 1, ifelse(Q62 %in% c(97,99), NA, 0)), 
      resi_efunds_firm_diff_4 = ifelse(Q62 == 4, 1, ifelse(Q62 %in% c(97,99), NA, 0)), 

      resi_efunds_firm_str = case_when(
        resi_efunds_firm_diff_3 == 1 ~ "Very difficult", 
        resi_efunds_firm_diff_2 == 1 | resi_efunds_firm_diff_1 == 1 ~ "Somewhat or not difficult at all", 
        resi_efunds_firm_diff_4 == 1 ~ "Not possible"
      ), 
      
      resi_efunds_firm_source_family = ifelse(Q63 == 1, 1, ifelse(Q63 %in% c(97,99), NA, 0)), 
      resi_efunds_firm_source_work = ifelse(Q63 == 2, 1, ifelse(Q63 %in% c(97,99), NA, 0)), 
      resi_efunds_firm_source_firmcsh = ifelse(Q63 == 3, 1, ifelse(Q63 %in% c(97,99), NA, 0)), 
      resi_efunds_firm_source_hhcsh = ifelse(Q63 == 4, 1, ifelse(Q63 %in% c(97,99), NA, 0)), 
      resi_efunds_firm_source_borrow = ifelse(Q63 == 5, 1, ifelse(Q63 %in% c(97,99), NA, 0)), 
      resi_efunds_firm_source_sellassets = ifelse(Q63 == 6, 1, ifelse(Q63 %in% c(97,99), NA, 0)), 
      resi_efunds_firm_source_other = ifelse(Q63 == 7, 1, ifelse(Q63 %in% c(97,99), NA, 0)), 
      
      # Asked only to owners
      resi_efunds_hh_owner_yes = ifelse(Q64 == 1, 1, ifelse(Q64 %in% c(97,99), NA, 0)), 
      resi_efunds_hh_owner_yes = ifelse(resp_owner == 0, 0, resi_efunds_hh_owner_yes), 
      resi_efunds_hh_owner_no = ifelse(Q64 == 2, 1, ifelse(Q64 %in% c(97,99), NA, 0)), 
      resi_efunds_hh_owner_no = ifelse(resp_owner == 0, 0, resi_efunds_hh_owner_no), 
      
      resi_suppchain_cust_diff_1 = ifelse(Q65 == 1, 1, ifelse(Q65 %in% c(97,99), NA, 0)), 
      resi_suppchain_cust_diff_2 = ifelse(Q65 %in% c(2, 3), 1, ifelse(Q65 %in% c(97,99), NA, 0)), 
      resi_suppchain_cust_diff_3 = ifelse(Q65 == 4, 1, ifelse(Q65 %in% c(97,99), NA, 0)), 
      
      resi_resp_attitude_chllng_str = names(attributes(Q66)$labels[Q66]),
      resi_resp_attitude_chllng_str = ifelse(Q66 %in% c(97,99), NA, resi_resp_attitude_chllng_str), 
      
      resi_resp_attitude_chllng_1 = ifelse(Q66 == 1, 1, ifelse(Q66 %in% c(97,99), NA, 0)), 
      resi_resp_attitude_chllng_2 = ifelse(Q66 == 2, 1, ifelse(Q66 %in% c(97,99), NA, 0)), 
      resi_resp_attitude_chllng_3 = ifelse(Q66 == 3, 1, ifelse(Q66 %in% c(97,99), NA, 0)), 
      resi_resp_attitude_chllng_4 = ifelse(Q66 == 4, 1, ifelse(Q66 %in% c(97,99), NA, 0)), 
      
      resi_network_conf_3 = ifelse(Q67 == 1, 1, ifelse(Q67 %in% c(97,99), NA, 0)), 
      resi_network_conf_2 = ifelse(Q67 %in% c(2, 3), 1, ifelse(Q67 %in% c(97,99), NA, 0)), 
      resi_network_conf_1 = ifelse(Q67 == 4, 1, ifelse(Q67 %in% c(97,99), NA, 0)),
      
      # Resilience capital
      # Financial: Can raise emergency funds in 7 days without major difficulty
      resi_capital_financial = ifelse(resi_efunds_firm_str == "Somewhat or not difficult at all", 1, 0), 
      # Dependency on customers: Can overcome challenges with main customer or supplier without major difficulty
      resi_capital_linkages = ifelse(resi_suppchain_cust_diff_1 == 1 | resi_suppchain_cust_diff_2 == 1, 1, 0), 
      # Dependency on household: Business savings not vulnerable to household shocks
      resi_capital_hh = ifelse(resi_efunds_hh_owner_no == 1, 1, 0), 
      # Confidence in network 
      resi_capital_network = ifelse(resi_network_conf_2 == 1 | resi_network_conf_3 == 1, 1, 0), 
      
      resi_capital_score_v1 = resi_capital_financial + resi_capital_linkages + resi_capital_hh + resi_capital_network, 
      resi_capital_score_v2 = resi_capital_financial + resi_capital_linkages + resi_capital_network
      
    ) %>%
    dummy_cols(select_columns = c("business_premise_shc", "business_size_agg2_shc", "business_sector_agg2_shc", "business_sector_agg3_shc",
                                  "resp_experience_agg5_shc", "resp_education_agg4_shc", "resp_age_agg6_shc", "resp_psych_segment_shc", 
                                  "tech_uses_messaging_shc", "tech_uses_socialmedia_shc", "tech_uses_ecommerce_shc", "tech_uses_software_shc",
                                  "risk_largestimpact_shc")) %>%
    mutate(

      # Index of how closely tied up (only defined for respondents that are owners) business is with owner's personal finances
      business_hh_noboundaries = ifelse(business_premise_shc_1 == 1 & resp_pcthhincfrmbus_high == 1 & business_finances_notseparate == 1, 1, 0),
      business_hh_noboundaries = ifelse(resp_owner == 1, business_hh_noboundaries, NA),
      business_hh_codep_index = business_premise_shc_1 + resp_pcthhincfrmbus_high + business_finances_notseparate,
      business_hh_codep_index = ifelse(resp_owner == 1, business_hh_codep_index, NA),
      business_hh_codep_index = business_hh_codep_index/3 # Re-scaling so that index has range of 0-1

    ) %>%

    select(starts_with(c("country", "ID", "Initial_block_ID", "Cluster_number", "fullsample", "weight_msme", "w", "business_", "resp_", "tech_", "perf_", "risk_", "resi_")))

}

runpca_psych <- function(data) { 
  
  prcomp(~ resp_motivation_entrep + resp_motivation_lackopp + resp_maingoal_grow + resp_maingoal_stab + resp_riskapproach_aggr + resp_riskapproach_avoid, scale = TRUE, data = data)

  }

runmca_psych <- function(data, showgraph = FALSE) { 
  
  data %>% 
    select(resp_motivation, resp_maingoal, resp_riskapproach, resi_resp_attitude_chllng_str) %>% filter(complete.cases(.)) %>% 
    mutate(
      resp_motivation = ifelse(resp_motivation == "Lack of other income opportunities or desire to generate additional income", "Lack other income opportunities", resp_motivation), 
      resp_maingoal = ifelse(resp_maingoal == "Leave business to get a regular wage paying job or other", "Leave business to get job", resp_maingoal), 
      resp_riskapproach = ifelse(resp_riskapproach == "Aggresively pursue risky opportunities that could generate significant additional income", "Aggressively pursue risky opportunities", resp_riskapproach), 
      resp_riskapproach = ifelse(resp_riskapproach == "Cautiously pursue risk opportunities that could generate a little income", "Cautiously pursue risky opportunities", resp_riskapproach), 
      resp_riskapproach = ifelse(resp_riskapproach == "Avoid risks even if they could generate additional income", "Avoid risks", resp_riskapproach), 
    ) %>%
    mutate(resp_motivation = str_wrap(resp_motivation, 20), 
           resp_maingoal = str_wrap(resp_maingoal, 20), 
           resp_riskapproach = str_wrap(resp_riskapproach, 20), 
           resi_resp_attitude_chllng_str = str_wrap(resi_resp_attitude_chllng_str, 20)) %>% 
    rename(Motivation = resp_motivation, `Main goal` = resp_maingoal, `Risk approach` = resp_riskapproach, `Reaction to challenge` = resi_resp_attitude_chllng_str) -> data
  
  MCA(data, graph = showgraph)
  
}

add_pca_todata <- function(data) { 
  
  subset <- data %>% select(ID, resp_motivation, resp_maingoal, resp_riskapproach, resi_resp_attitude_chllng_str) %>% filter(complete.cases(.))
  mca <- runmca_psych(subset, showgraph = FALSE)
  dims <- mca$ind$coord[, c(1, 2)]
  
  subset %>% 
    mutate(
      psych_mca_dim1 = dims[, 1], 
      psych_mca_dim2 = dims[, 2]
    ) %>% 
    select(ID, psych_mca_dim1, psych_mca_dim2) -> subset
  
  return(data %>% left_join(subset, by = "ID"))
  
  }

# Function to compute summary statistics -------------------

compute_summary_clusterlevel_1g <- function(inidcators, groups, data, weights, psu = NULL, keep = NULL) {

  data <- data %>%
    left_join(weights, by = c("Initial_block_ID")) %>%
    mutate(fullsample = "All businesses")

  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  results <- dplyr::bind_rows(
  map2(is,
       gs,
       svy_summary_1g,
       data = data,
       iref = INDICATORS,
       gref = GROUPS,
       psu = psu,
       strata = NULL,
       w = "weight_cluster")
  )

  if (!is.null(keep)) {
    return(results %>% select(indicator, indicator_name, group, group_name, group_cat_val, nobs, starts_with(keep)))
  } else {
    return(results)
  }

}

compute_summary_clusterlevel_2g <- function(inidcators, groups_l1, groups_l2, data, weights, psu = NULL, keep = NULL) {

  data <- data %>%
    left_join(weights, by = c("Initial_block_ID")) %>%
    mutate(fullsample = "All businesses")

  combinations <- expand.grid(indicators, names(groups_l2), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_2g,
         data = data,
         iref = INDICATORS,
         g_l1 = groups_l1,
         gref = GROUPS,
         psu = psu,
         strata = NULL,
         w = "weight_cluster")
  )

  if (!is.null(keep)) {
    return(results %>% select(!!sym(groups_l1), indicator, indicator_name, group, group_name, group_cat_val, nobs, starts_with(keep)))
  } else {
    return(results)
  }

}

compute_summary_mainlevel_1g <- function(inidcators, groups, data, weights, psu = NULL, keep = NULL) {

  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_1g,
         data = data,
         iref = INDICATORS,
         gref = GROUPS,
         psu = psu,
         strata = NULL,
         w = weights)
  )

  if (!is.null(keep)) {
    return(results %>% select(indicator, indicator_name, group, group_name, group_cat_val, nobs, starts_with(keep)))
  } else {
    return(results)
  }

}

compute_summary_mainlevel_2g <- function(inidcators, groups_l1, groups_l2, data, weights, psu = NULL, keep = NULL) {

  combinations <- expand.grid(indicators, names(groups_l2), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_2g,
         data = data,
         iref = INDICATORS,
         g_l1 = groups_l1,
         gref = GROUPS,
         psu = psu,
         strata = NULL,
         w = weights)
  )

  if (!is.null(keep)) {
    return(results %>% select(!!sym(groups_l1), indicator, indicator_name, group, group_name, group_cat_val, nobs, starts_with(keep)))
  } else {
    return(results)
  }

}


# Function to create a correlation matrix of select indicators  -------------------

gen_corrmatrix <- function(data, indicators) {

  dat <- main_data[, indicators]
  dat <- dat[complete.cases(dat), ]

  rcorr(as.matrix(dat))

}

# Function to run regressions -------------------

capture_terms <- function(depvar, maineffect, confounds, data) {

  # Function captures the effects of simple linear regression model

  if (!is.null(confounds)) {
    f <- as.formula(paste(depvar, "~", paste(c(maineffect, confounds), collapse = "+")))
    flag <- 1
  } else {
    f <- as.formula(paste(depvar, "~", maineffect))
    flag <- 0
  }

  lm_mod <- linear_reg()
  lm_fit <- lm_mod %>% fit(f, data = data)
  rsq <- glance(lm_fit)$adj.r.squared
  tidy(lm_fit) %>%
    mutate(adj_rsquared = rsq, depvar = depvar, confounds_flag = flag) %>%
    filter(term %in% c("(Intercept)", maineffect))

}

capture_terms_clse <- function(depvar, maineffects, confounds, data) {

  # Function captures the effects of simple linear regression model with clustered standard errors

  if (!is.null(confounds)) {
    f <- as.formula(paste(depvar, "~", paste(c(maineffects, confounds), collapse = "+")))
    flag <- 1
  } else {
    if (length(maineffects) > 1) {
      f <- as.formula(paste(depvar, "~", paste(c(maineffects), collapse = "+")))
    } else {
      f <- as.formula(paste(depvar, "~", maineffects))
    }
    flag <- 0
  }

  lm_fit <- feols(f, cluster = ~ Initial_block_ID, data = data)
  rsq <- glance(lm_fit)$adj.r.squared
  tidy(lm_fit) %>%
    mutate(adj_rsquared = rsq, depvar = depvar, confounds_flag = flag) %>%
    filter(term %in% c("(Intercept)", maineffects))

}

prep_fig <- function(terms, depvar_labels, effect_labels) {

  terms %>% mutate(
    depvar_label = factor(depvar_labels[depvar], levels = depvar_labels, ordered = TRUE),
    effect_label = factor(effect_labels[term], levels = effect_labels, ordered = TRUE),
    fig_data = max(ifelse(term == "(Intercept)", estimate, NA), na.rm = TRUE),
    fig_data = ifelse(term != "(Intercept)", estimate + fig_data, estimate),
    model_type = factor(ifelse(confounds_flag == 0, "Model: Unadjusted", "Model: Adjusted"), levels = c("Model: Unadjusted", "Model: Adjusted"), ordered = TRUE)
  )

}

model_and_prepfig <-function(depvar, maineffect, confounds, data, depvar_labels, effect_labels) {
  terms <- capture_terms_clse(depvar, maineffect, confounds, data)
  fig <- prep_fig(terms, depvar_labels, effect_labels)
  return(fig)
}






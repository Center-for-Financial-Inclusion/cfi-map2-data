

# Functions to retrieve data ------------

get_raster <- function(country) { 
  
  filename <- glue("data/{country}/response_wbuffer.tiff")
 
  raster_img <- stack(filename)
  
  return(raster_img)
  
}


get_sampling_grid <- function(country) {

  # Gets full list of initial sampled blocks for specified country

  filename <- glue("https://raw.githubusercontent.com/pgubb/cfi-map2-sampling-grids/main/outputs/{country}/final_sampling_grid.geojson")

  polygons <- st_read(filename, quiet = TRUE)
  
  return(polygons)
  
  }

get_boundary <- function(polys) { 
  
  st_simplify(polys %>% summarise()%>% st_concave_hull(ratio=0.1) %>% st_cast(to ="LINESTRING")) %>% st_as_sfc()
  
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

# Functions to prepare enumeration data and weights ----------

prep_enumeration_data <- function(country) {

  filename <- glue("data/{country}/TS_{country}.sav")

  data <- read_sav(filename)

  data <- data %>%
    mutate(
      country = country,
      fullsample = "All businesses",
      business_total = 1,
      business_eligible = ifelse(Eligibility == 1, 1, 0),
      business_selected = ifelse(Selected == 1, 1, 0),
      business_interviewed = ifelse(Completed == 1, 1, 0),
      #Q1 = ifelse(Q1 == -1, 4, Q1),
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
      business_sector_agg3 = ifelse(is.na(business_sector_agg3), "Don't know", business_sector_agg3), 
      business_sector_agg4 = case_when(
        Q2 %in% c(2, 8, 9, 10) ~ "Food production and distribution", 
        Q2 %in% c(11, 12, 13) ~ "Non-food retail trade", 
        Q2 %in% c(1, 3, 4, 5, 6, 7) ~ "Non-food manufacturing",
        Q2 %in% c(14, 15, 16, 17, 18, 19, 20, 21, 22) ~ "Other services", 
        Q2 %in% c(97) ~ "Don't know"
      )
    ) 

    if(country %in% c("India", "Brazil")) {
      data %>% rename(BlockID = Block_ID) -> data
      }

    return(data)

  }

# For India & Brazil creating cross-link between BlockID and Initial_Block_ID
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
    n = weight_params[[country]][["n"]]
  }
  
  business_data %>%
    left_join(blocks_data, by = "Initial_block_ID") %>%
    # Mapping parameters for computing weights (per document) to variables
    mutate(
      # For blocks that were not in the tracking sheet, these were visited but no businesses were found (i.e. residential etc)
      N_blocks_percluster = ifelse(is.na(N_blocks_percluster), 1, N_blocks_percluster),
      N = weight_params[[country]][["N"]],
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

get_main_data <- function(country, enum_data = NULL) {
  
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
    
  } else if (country %in% c("India", "Brazil")) {
    
      data %>%
        left_join(blockID_to_InitialID(enum_data), by = c("BlockID")) -> data
    
    }

  return(data)

}

correct_education <- function(data, selected_country) { 
  
  if (selected_country == "Indonesia") {
  # This standardizes the education labels for Indonesia with those in the other countries
    data %>% mutate(
      Q73 = 
      case_match(
        Q73, 
         1 ~ 1, 
        2 ~ 2, 
        3 ~ 3, 
        c(4, 5, 6) ~ 4, # Some senior high school to 4 some secondary
        7 ~ 5, # Completed senior high school to 5 completed secondary, 
        8 ~ 8, 
        9 ~ 9, 
        10 ~ 10,
        11 ~ 7 # Completed university to correct value label
    )
    )
  } else { 
    data
    }

}

replace_negatives <- function(x) { 
  ifelse(x == -1, NA, x)
}

prep_main_data <- function(raw_data, weights, selected_country) {
  
  if (selected_country == "Nigeria") { 
    add_worker <- 1
  } else { 
    add_worker <- 0
    }
  
  # Correcting education labels for Indonesia
  raw_data <- raw_data %>% correct_education(selected_country = selected_country)
  
  # Creating select variables that are affected by subsequent modification for brazil
  
  raw_data <- raw_data %>% 
    mutate(
      business_sector_str = names(attributes(Q2)$labels[Q2]),
      business_registration_status = names(attributes(Q74)$labels[Q74]),
      business_premise = names(attributes(Q1)$labels[Q1]),
      Q77 = ifelse(Q77 < 0, NA, Q77), 
      resp_pcthhincfrmbus = names(attributes(Q77)$labels[Q77]),
      risk_largestimpact_str = names(attributes(Q56)$labels[Q56]),
      risk_largestimpact_str = ifelse(Q56 == 99, NA, risk_largestimpact_str), 
    )
  
  if (selected_country == "Brazil") { 
    raw_data <- raw_data %>% mutate_if(is.numeric, replace_negatives)
  }
  
  raw_data %>%

    left_join(weights, by = join_by(Initial_block_ID)) %>%

    mutate(
      
      fullsample = "All businesses",
      business_total = 1,

      # Firm-level characteristics --------
      business_premise_shc = Q1,
      
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

      business_sector_food = ifelse(Q2 %in% c(2, 8, 9, 10), 1, 0), 
      
      business_sector_agg_food_str = ifelse(Q2 %in% c(2, 8, 9, 10), "Food production and distribution", "Other"), 
      
      business_sector_agg4 = case_when(
        Q2 %in% c(2, 8, 9, 10) ~ "Food production and distribution", 
        Q2 %in% c(11, 12, 13) ~ "Non-food retail trade", 
        Q2 %in% c(1, 3, 4, 5, 6, 7) ~ "Non-food manufacturing",
        Q2 %in% c(14, 15, 16, 17, 18, 19, 20, 21, 22) ~ "Other services", 
        Q2 %in% c(97) ~ "Don't know"
        ), 
      
      # Adjusting weights -------
      adj_factor_sector = map2_dbl(country, business_sector_agg4, get_adj_factors, x = WEIGHT_ADJ_SECTOR),
      adj_factor_prem = map2_dbl(country, business_premise, get_adj_factors, x = WEIGHT_ADJ_PREM),
      weight_msme_adj_v0 = 1/(p1*(p2*adj_factor_sector)),
      weight_msme_adj = 1/(p1*(p2*adj_factor_sector*adj_factor_prem)),
      
      business_registered_yes = ifelse(Q74 == 1, 1, 0),
      business_registered_yes = ifelse(Q74 %in% c(97, 99), NA, business_registered_yes),
      business_registered_no = ifelse(Q74 == 2, 1, 0),
      business_registered_no = ifelse(Q74 %in% c(97, 99), NA, business_registered_no),
      business_registration_status = ifelse(Q77 == 1, "Yes", "No"), 
      business_registration_status = ifelse(Q77 %in% c(97,99), NA, business_registration_status), 
      #business_sector_agg3 = factor(business_sector_agg4, levels = c("Services: retail & wholesale trade", "Services: other", "Manufacturing", "Construction & other"), ordered = TRUE),

      business_size = ifelse(Q6 == 0, 1, Q6),
      business_size_agg2 = ifelse(business_size == 1, "1 person", "2-10 people"),
      business_size_agg2_shc = ifelse(business_size_agg2 == "1 person", "1", "2_10"),
      #business_size_agg2 = factor(business_size_agg2, levels = c("1 person", "2-10 people"), ordered = TRUE),

      # Respondent characteristics ---------
      resp_type_owner = ifelse(Q4 %in% c(1,3), 1, 0), # Is the respondent the owner or a manager?
      resp_type_manager = ifelse(Q4 %in% c(2,4), 1, 0), 
      resp_owner_str = case_when(
        Q4 %in% c(1, 3) ~ "Owner", 
        Q4 == 2 ~ "Manager", 
        Q4 == 4 ~ "Other"
      ), 
      
      resp_experience = ifelse(Q8 == 97, NA, Q8),
      resp_experience_c = resp_experience - mean(resp_experience, na.rm = TRUE),
      resp_experience_c_5yi = resp_experience_c/5,
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
      resp_age_c = resp_age - mean(resp_age, na.rm = TRUE), 
      resp_age_c_5yi = resp_age_c/5, 
      resp_age_agg3_shc = case_when(
        resp_age < 35 ~ "35", 
        resp_age >= 35 & resp_age < 55 ~ "3555", 
        resp_age >= 55 ~ "55"
      ), 
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
        resp_education <= 4 ~ "Primary",
        resp_education %in% c(5, 6, 8, 10) ~ "Secondary",
        resp_education %in% c(7, 9, 11) ~ "University/technical school"
      ),
      resp_education_agg4_shc = case_when(
        resp_education <= 2 ~ "non",
        resp_education <= 4 ~ "pri",
        resp_education %in% c(5, 6, 8, 10) ~ "sec",
        resp_education %in% c(7, 9, 11) ~ "trt"
      ),
      resp_education_agg5_shc = case_when(
        resp_education <= 2 ~ "non",
        resp_education <= 4 ~ "pri",
        resp_education %in% c(5, 6, 8, 10) ~ "sec",
        resp_education %in% c(9) ~ "trt_voc",
        resp_education %in% c(7, 11) ~ "trt_uni"
      ),
      resp_education_agg2_shc = ifelse(resp_education_agg4_shc %in% c("non", "pri"), "priorless", "secormore"), 
      resp_education_agg2 = ifelse(resp_education_agg4_shc %in% c("non", "pri"), "Educational attainment: Primary or less", "Educational attainment: Secondary or more"), 
      
      resp_education_agg2_alt = ifelse(resp_education_agg4_shc %in% c("non", "pri", "sec"), "Educational attainment: Secondary or less", "Educational attainment: Post-secondary"), 
      
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

      # Owner identifies as an entrepreneur, wants to grow by taking risks
      resp_psych_segment = ifelse(
              resp_motivation_entrep == 1 &
              resp_maingoal_grow == 1 &
              (resp_riskapproach_aggr == 1 | resp_riskapproach_calc == 1), "Growth", NA),
      # Owner started business du to lack of opportunities & wants to avoid risks
      resp_psych_segment = ifelse(
              resp_motivation_entrep != 1 &
              (resp_maingoal_leave == 1 | resp_maingoal_stab == 1) & 
              (resp_riskapproach_avoid == 1 | resp_riskapproach_calc == 1), "Survival", resp_psych_segment
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
      resp_pcthhincfrmbus_high = ifelse(Q77 %in% c(3,4), 1, 0),
      resp_pcthhincfrmbus_high = ifelse(Q77 %in% c(97,99), NA, resp_pcthhincfrmbus_high),

      # Business has formal account
      business_account_formal = ifelse(Q30 == 1, 1, 0),
      business_account_formal = ifelse(Q30 %in% c(97,99), NA, business_account_formal),

      # Owner's account separate from business account
      business_account_separate = case_match(Q31, 1 ~ 1, 2 ~ 0, 99 ~ NA),
      
      business_account_separate_v2 = ifelse(business_account_formal == 1 & business_account_separate == 1, 1, 0), 

      # Business has no formal account or business account is not sperate from owner's account
      business_finances_notseparate = ifelse(business_account_formal == 0 | business_account_separate == 0, 1, 0),

      #Digital technology --------

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
      tech_uses_digpayments = ifelse(Q35_3 == 1 | Q35_5 == 1 | Q35_6 == 1 | Q35_7 == 1 | Q35_8 == 1, 1, 0),

      tech_uses_digloans = ifelse(Q38_1 == 1 | Q38_2 == 1 | Q38_3 == 1 | Q38_4 == 1 | Q38_5 == 1 | Q38_6 == 1 |  Q37_3 == 1 | Q37_6 == 1 | Q37_7 == 1, 1, 0),
      tech_uses_digloans = ifelse(is.na(tech_uses_digloans), 0, tech_uses_digloans),

      tech_uses_diginsurance = ifelse(B2_1 == 1 | B2_2 == 1 | B2_3 == 1 | B2_4 == 1 | B2_5 == 1 | B2_6 == 1 | B2_7 == 1 | B2_8 == 1 | B2_9 == 1 | B2_10 == 1, 1, 0),
      tech_uses_diginsurance = ifelse(is.na(tech_uses_diginsurance), 0, tech_uses_diginsurance),
  
      tech_uses_adoption_score = tech_has_internet + tech_has_device + tech_uses_website + tech_uses_messaging + tech_uses_socialmedia + tech_uses_ecommerce + tech_uses_software + tech_uses_ai + tech_uses_digpayments + tech_uses_digloans,
      tech_uses_adoption_score_c = tech_uses_adoption_score - mean(tech_uses_adoption_score, na.rm = TRUE),
      tech_uses_adoption_score_norm = tech_uses_adoption_score/10,
      
      # User of non-financical technologies
      tech_nonfin_adopter = ifelse(Q15 == 1 | Q17 == 1 | Q19 == 1 | Q21 == 1 | Q23 == 1 | Q26 == 1, 1, 0), 
      
      # Factors encouraging adoption 
      tech_adopfactors_demand = ifelse(Q27_1 == 1, 1, 0), 
      tech_adopfactors_support = ifelse(Q27_2 == 1, 1, 0), 
      tech_adopfactors_comp = ifelse(Q27_3 == 1, 1, 0), 
      tech_adopfactors_other = ifelse(Q27_4 == 1, 1, 0), 
      
      # Benefits 
      tech_adopbenefits_rev = ifelse(Q28_1 == 1, 1, 0), 
      tech_adopbenefits_costs = ifelse(Q28_2 == 1, 1, 0), 
      tech_adopbenefits_customer = ifelse(Q28_3 == 1, 1, 0), 
      tech_adopbenefits_sales = ifelse(Q28_4 == 1, 1, 0), 
      tech_adopbenefits_cashflow = ifelse(Q28_5 == 1, 1, 0), 
      tech_adopbenefits_finserv = ifelse(Q28_6 == 1, 1, 0), 
      tech_adopbenefits_other = ifelse(Q28_7 == 1, 1, 0),
      
      # Challenges 
      tech_adopchllng_diff = ifelse(Q29_1 == 1, 1, 0), 
      tech_adopchllng_cost = ifelse(Q29_2 == 1, 1, 0), 
      tech_adopchllng_trust = ifelse(Q29_3 == 1, 1, 0), 
      tech_adopchllng_notrlv = ifelse(Q29_4 == 1, 1, 0), 
      tech_adopchllng_infra = ifelse(Q29_5 == 1, 1, 0), 
      tech_adopchllng_other = ifelse(Q29_6 == 1, 1, 0), 
      
      # Functional perspective
      
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
      
      # Combining access to markets or operations 
      tech_function_mkts_ops = ifelse(tech_function_mkts == 1 | tech_function_ops == 1, 1, 0), 
      
      # Enterprise digital finance  
      tech_function_epay = tech_uses_digpayments,
      tech_function_epay_expos = ifelse(Q35_5 == 1 | Q35_6 == 1 | Q35_7 == 1 | Q35_8 == 1, 1, 0),
      tech_function_efin = ifelse(tech_uses_digloans == 1 | tech_uses_diginsurance == 1, 1, 0),

      tech_function_total = tech_function_comms + tech_function_mkts + tech_function_ops + tech_function_efin,
      tech_function_30da_total = tech_function_comms_30da + tech_function_mkts_30da + tech_function_ops_30da,

      tech_cat_none = ifelse(tech_function_comms == 0 & tech_function_mkts == 0 & tech_function_ops == 0 & tech_function_epay == 0, 1, 0),
      tech_cat_any1 = ifelse(tech_function_total == 1, 1, 0),
      tech_cat_any2 = ifelse(tech_function_total == 2, 1, 0),
      tech_cat_any3 = ifelse(tech_function_total == 3, 1, 0),
      tech_cat_all4 = ifelse(tech_function_total == 4, 1, 0),
      tech_cat_any3all4 = ifelse(tech_function_total %in% c(3,4), 1, 0),
      
      tech_function_index = (tech_function_total + tech_function_30da_total)/7,

      tech_function_any = ifelse(tech_function_comms == 1 | tech_function_mkts == 1 | tech_function_ops == 1 | tech_function_epay == 1, 1, 0), 
      
      # Business performance ----------
      
      fx = unlist(FX_RATES[country]),
      ppp = unlist(PPP_RATES[country]), 
      perf_mpy = Q75, # Months per year of operations
      perf_mpy = ifelse(Q75 %in% c(97, 99), NA, perf_mpy),
      perf_hrspw = Q76, # hourse per week of operation
      perf_hrspw = ifelse(Q76 %in% c(97, 99), NA, perf_hrspw),
      perf_rev = as.numeric(zap_labels(raw_data$Q78)),
      perf_rev = ifelse(Q78 %in% c(97,99), NA, perf_rev),
      perf_rev_reports = ifelse(!is.na(perf_rev), 1, 0), 
      perf_rev_usd = perf_rev/fx,
      perf_rev_ppp = perf_rev/ppp,
      perf_hrspy = perf_hrspw*4*perf_mpy, #Total hours per year of operation
      perf_hrspm = perf_hrspy/12, #Avg. Hours per month of operation
      perf_hrspd = perf_hrspm/30.5, #Avg. Hours per day of operation
      perf_revphr = perf_rev/perf_hrspm,
      perf_revphrpemp = perf_revphr/business_size, # Revenue per hour per employee
      perf_revphrpemp_usd = perf_revphrpemp/fx, # Revenue per hour per employee
      perf_revphrpemp_ppp = perf_revphrpemp/ppp, # Revenue per hour per employee
      perf_revpdypemp_usd = (perf_revphrpemp*8)/fx, # Revenue per day per employee
      perf_revpdypemp_usd_log = log(perf_revpdypemp_usd), # Revenue per day per employee
      perf_revpdypemp_ppp = (perf_revphrpemp*8)/ppp, # Revenue per day per employee
      perf_revpdypemp_ppp_log = log(perf_revpdypemp_ppp), 
      
      perf_sales_up = ifelse(Q68 == 1, 1, 0), 
      perf_sales_up = ifelse(Q68 %in% c(97,99), NA, perf_sales_up), 
      perf_sales_same = ifelse(Q68 == 2, 1, 0), 
      perf_sales_same = ifelse(Q68 %in% c(97,99), NA, perf_sales_same),  
      perf_sales_down = ifelse(Q68 == 3, 1, 0), 
      perf_sales_down = ifelse(Q68 %in% c(97,99), NA, perf_sales_down), 
        
      perf_emp_up = ifelse(Q69 == 1, 1, 0), 
      perf_emp_up = ifelse(Q69 %in% c(97,99), NA, perf_emp_up), 
      perf_emp_same = ifelse(Q69 == 2, 1, 0), 
      perf_emp_same = ifelse(Q69 %in% c(97,99), NA, perf_emp_same),  
      perf_emp_down = ifelse(Q69 == 3, 1, 0), 
      perf_emp_down = ifelse(Q69 %in% c(97,99), NA, perf_emp_down), 
      
      perf_investment = ifelse(Q70 == 1, 1, 0), 
      perf_investment = ifelse(Q70 %in% c(97,99), NA, perf_investment), 
      
      perf_newservices = ifelse(Q71 == 1, 1, 0), 
      perf_newservices = ifelse(Q71 %in% c(97,99), NA, perf_newservices), 
      
      perf_growth_subj_any = ifelse(perf_sales_up == 1 | perf_emp_up == 1, 1, 0), 
  
      perf_growthdyn_subj_score = perf_sales_up + perf_emp_up + perf_investment + perf_newservices, 
      
      perf_subj_growing = ifelse((perf_sales_up == 1 | perf_emp_up == 1) & (perf_investment == 1 | perf_newservices == 1), 1, 0), 
      perf_subj_stable = ifelse(perf_sales_same == 1 & perf_emp_same == 1 & perf_investment == 0 & perf_newservices == 0, 1, 0), 
      perf_subj_struggle = ifelse((perf_sales_down == 1 | perf_emp_down == 1) & perf_investment == 0 & perf_newservices == 0, 1, 0), 
      perf_subj_other = ifelse(perf_subj_growing == 0 & perf_subj_stable == 0 & perf_subj_struggle, 1, 0), 
      
      # Financial services ----------------
      
      # Business has formal account
      #D oes this business currently use a formal account with a financial institution to manage its finances and payments in the business name or a person's name?
      fin_account_formal = ifelse(Q30 == 1, 1, 0),
      fin_account_formal = ifelse(Q30 %in% c(97,99), NA, fin_account_formal),
      
      # Expanded definition of formal account (to include savings in a financial institution account)
      fin_account_formal_v2 = ifelse(fin_account_formal == 1 | 
                                       Q33 %in% c(1, 2, 3, 4, 6), 1, 0), 
      
      # Expanded definition of formal account to include, savings and non-cash payments 
      fin_account_formal_v3 = ifelse(fin_account_formal_v2 == 1 | 
                                      tech_uses_digpayments == 1 | 
                                      Q35_4 == 1, 1, 0),
      
      # Expanded definition of formal account to include, savings, payments and formal loans
      fin_account_formal_v4 = ifelse(fin_account_formal_v3 == 1 | 
                                      Q37_1 == 1 | # Commercial bank loan
                                      Q37_2 == 1 | # MFI loan 
                                      Q37_3 == 1 | 
                                      Q37_4 == 1 | 
                                      Q37_6 == 1, 1, 0),
      
      # Definition of usage of formal account to include savings, payments and formal loans
      fin_account_formal_usgdef = ifelse(
        Q33 %in% c(1, 2, 3, 4, 6) | # Business with a formal financial institution or mobile money wallet
        (Q35_4 == 1 | Q35_6 == 1) | # Accepts payment via bank transfers or mobile money
        (Q37_1 == 1 | Q37_2 == 1 | Q37_4 == 1 | Q37_6 == 1),  # Has an active loan from a commercial bank, mfi, sacco or mobile money
        1, 0),
      
      # Can owner save regularly out of business income
      fin_owner_save = ifelse(Q32 == 1, 1, 0),
      fin_owner_save = ifelse(Q32 %in% c(97,99), NA, fin_owner_save),
      
      # Business savings account/method
      fin_bus_savings_cbnk = ifelse(Q33 == 1, 1, 0), 
      fin_bus_savings_cbnk = ifelse(Q33 %in% c(97,99), NA, fin_bus_savings_cbnk),
      fin_bus_savings_mfi =  ifelse(Q33 == 2, 1, 0), 
      fin_bus_savings_mfi = ifelse(Q33 %in% c(97,99), NA, fin_bus_savings_mfi),
      fin_bus_savings_fintech =  ifelse(Q33 == 3, 1, 0), 
      fin_bus_savings_fintech = ifelse(Q33 %in% c(97,99), NA, fin_bus_savings_fintech),
      fin_bus_savings_sacco =  ifelse(Q33 == 4, 1, 0), 
      fin_bus_savings_sacco = ifelse(Q33 %in% c(97,99), NA, fin_bus_savings_sacco),
      fin_bus_savings_group =  ifelse(Q33 == 5, 1, 0), 
      fin_bus_savings_group = ifelse(Q33 %in% c(97,99), NA, fin_bus_savings_group),
      fin_bus_savings_mm =  ifelse(Q33 == 6, 1, 0), 
      fin_bus_savings_mm = ifelse(Q33 %in% c(97,99), NA, fin_bus_savings_mm),
      fin_bus_savings_other =  ifelse(Q33 == 7, 1, 0), 
      fin_bus_savings_other = ifelse(Q33 %in% c(97,99), NA, fin_bus_savings_other),
      
      fin_bus_savings_agg_fi = ifelse(fin_bus_savings_cbnk == 1 | fin_bus_savings_mfi == 1 | fin_bus_savings_sacco == 1, 1, 0), 
      fin_bus_savings_agg_nbfi = ifelse(fin_bus_savings_fintech == 1 | fin_bus_savings_mm == 1, 1, 0), 
      fin_bus_savings_agg_inf = ifelse(fin_bus_savings_group == 1 | fin_bus_savings_other == 1, 1, 0), 
      
      # Transaction channels available for savings account
      fin_bus_savings_channel_inprsn = ifelse(Q34 == 1, 1, 0), 
      fin_bus_savings_channel_inprsn = ifelse(Q34 %in% c(97,99), NA, fin_bus_savings_channel_inprsn), 
      fin_bus_savings_channel_digonly = ifelse(Q34 == 2, 1, 0),
      fin_bus_savings_channel_digonly = ifelse(Q34 %in% c(97,99), NA, fin_bus_savings_channel_digonly), 
      fin_bus_savings_channel_both = ifelse(Q34 == 3, 1, 0),
      fin_bus_savings_channel_both = ifelse(Q34 %in% c(97,99), NA, fin_bus_savings_channel_both), 
      fin_bus_savings_channel_other = ifelse(Q34 == 4, 1, 0),
      fin_bus_savings_channel_other = ifelse(Q34 %in% c(97,99), NA, fin_bus_savings_channel_other), 

      # Customer payment acceptance mechanisms
      fin_merchpay_cashcheques = ifelse(Q35_1 == 1 | Q35_2 == 1, 1, 0),  
      fin_merchpay_poscard = ifelse(Q35_3 == 1, 1, 0),  
      fin_merchpay_poscard = ifelse(Q35_3 == 3, NA, fin_merchpay_poscard), 
      fin_merchpay_bnktrnsf = ifelse(Q35_4 == 1, 1, 0),  
      fin_merchpay_bnktrnsf = ifelse(Q35_4 == 3, NA, fin_merchpay_bnktrnsf), 
      fin_merchpay_online = ifelse(Q35_5 == 1, 1, 0),  
      fin_merchpay_online = ifelse(Q35_5 == 3, NA, fin_merchpay_online), 
      fin_merchpay_mobmoney = ifelse(Q35_6 == 1, 1, 0),  
      fin_merchpay_mobmoney = ifelse(Q35_6 == 3, NA, fin_merchpay_mobmoney), 
      fin_merchpay_instant = ifelse(Q35_7 == 1, 1, 0),  
      fin_merchpay_instant = ifelse(Q35_7 == 3, NA, fin_merchpay_instant), 
      fin_merchpay_qr = ifelse(Q35_8 == 1, 1, 0),  
      fin_merchpay_qr = ifelse(Q35_8 == 3, NA, fin_merchpay_qr), 
      
      fin_merchpay_agg_fi = ifelse(Q35_2 == 1 | fin_merchpay_bnktrnsf == 1 | fin_merchpay_poscard ==1, 1, 0), 
      fin_merchpay_agg_nbfi = ifelse(fin_merchpay_online == 1 | fin_merchpay_mobmoney == 1 | fin_merchpay_instant == 1 | fin_merchpay_qr ==1, 1, 0), 
      fin_merchpay_agg_cash = ifelse(Q35_1 == 1, 1, 0), 
      
      fin_merchpay_noncash = ifelse(fin_merchpay_poscard == 1 | 
                                      fin_merchpay_bnktrnsf == 1 | 
                                      fin_merchpay_online == 1 | 
                                      fin_merchpay_mobmoney == 1 | 
                                      fin_merchpay_instant == 1 | 
                                      fin_merchpay_qr == 1, 1, 0), 
      
      fin_merchpay_extrnsfpos = ifelse(fin_merchpay_online == 1 | 
                                      fin_merchpay_mobmoney == 1 | 
                                      fin_merchpay_instant == 1 | 
                                      fin_merchpay_qr == 1, 1, 0), 
      
      # Owner used a credit card for business purposes
      fin_owner_creditcard = ifelse(Q36 == 1, 1, 0), 
      fin_owner_creditcard = ifelse(Q36 %in% c(97,99), NA, fin_owner_creditcard), 
      
      # Active loans in las 12 months
      fin_activeloan_cbnk = ifelse(Q37_1 == 1, 1, 0),
      fin_activeloan_cbnk = ifelse(Q37_1 %in% c(97,99), NA, fin_activeloan_cbnk), 
      fin_activeloan_mfi = ifelse(Q37_2 == 1, 1, 0), 
      fin_activeloan_mfi = ifelse(Q37_2 %in% c(97,99), NA, fin_activeloan_mfi), 
      fin_activeloan_fintech = ifelse(Q37_3 == 1, 1, 0), 
      fin_activeloan_fintech = ifelse(Q37_3 %in% c(97,99), NA, fin_activeloan_fintech), 
      fin_activeloan_sacco = ifelse(Q37_4 == 1, 1, 0), 
      fin_activeloan_sacco = ifelse(Q37_4 %in% c(97,99), NA, fin_activeloan_sacco), 
      fin_activeloan_group = ifelse(Q37_5 == 1, 1, 0), 
      fin_activeloan_group = ifelse(Q37_5 %in% c(97,99), NA, fin_activeloan_group), 
      fin_activeloan_mm = ifelse(Q37_6 == 1, 1, 0), 
      fin_activeloan_mm = ifelse(Q37_6 %in% c(97,99), NA, fin_activeloan_mm), 
      fin_activeloan_platform = ifelse(Q37_7 == 1, 1, 0), 
      fin_activeloan_platform = ifelse(Q37_7 %in% c(97,99), NA, fin_activeloan_platform), 
      fin_activeloan_supplier = ifelse(Q37_8 == 1, 1, 0), 
      fin_activeloan_supplier = ifelse(Q37_8 %in% c(97,99), NA, fin_activeloan_supplier), 
      fin_activeloan_moneylndr = ifelse(Q37_9 == 1, 1, 0), 
      fin_activeloan_moneylndr = ifelse(Q37_9 %in% c(97,99), NA, fin_activeloan_moneylndr), 
      fin_activeloan_ff = ifelse(Q37_10 == 1, 1, 0), 
      fin_activeloan_ff = ifelse(Q37_10 %in% c(97,99), NA, fin_activeloan_ff), 
      fin_activeloan_other = ifelse(Q37_11 == 1, 1, 0), 
      fin_activeloan_other = ifelse(Q37_11 %in% c(97,99), NA, fin_activeloan_other), 
      
      fin_activeloan_agg_fi = ifelse(fin_activeloan_cbnk == 1 | fin_activeloan_mfi == 1 | fin_activeloan_sacco == 1 | fin_owner_creditcard == 1, 1, 0), 
      fin_activeloan_agg_nbfi = ifelse(fin_activeloan_fintech == 1 | fin_activeloan_mm == 1 | fin_activeloan_platform == 1, 1, 0), 
      fin_activeloan_agg_inf = ifelse(fin_activeloan_ff == 1 | fin_activeloan_group == 1 | fin_activeloan_supplier == 1 | fin_activeloan_moneylndr == 1, 1, 0), 
      fin_activeloan_agg_any = ifelse(Q37_1 == 1 | Q37_2 == 1 | Q37_3 == 1 | Q37_4 == 1 | Q37_5 == 1 | Q37_6 == 1 | Q37_7 == 1 | Q37_8 == 1 | Q37_9 == 1 | Q37_10 == 1 | Q37_11 == 1, 1, 0), 
      
      # Did business apply for loan digitally? 
      fin_activeloan_da_cbnk = ifelse(Q38_1 == 1, 1, 0), 
      fin_activeloan_da_cbnk = ifelse(Q38_1 %in% c(97,99), NA, fin_activeloan_da_cbnk), 
      fin_activeloan_da_mfi = ifelse(Q38_2 == 1, 1, 0), 
      fin_activeloan_da_mfi = ifelse(Q38_2 %in% c(97,99), NA, fin_activeloan_da_mfi), 
      fin_activeloan_da_fintech = ifelse(Q38_3 == 1, 1, 0), 
      fin_activeloan_da_fintech = ifelse(Q38_3 %in% c(97,99), NA, fin_activeloan_da_fintech), 
      fin_activeloan_da_sacco = ifelse(Q38_4 == 1, 1, 0), 
      fin_activeloan_da_sacco = ifelse(Q38_4 %in% c(97,99), NA, fin_activeloan_da_sacco), 
      fin_activeloan_da_group = ifelse(Q38_5 == 1, 1, 0), 
      fin_activeloan_da_group = ifelse(Q38_5 %in% c(97,99), NA, fin_activeloan_da_group), 
      fin_activeloan_da_mm = ifelse(Q38_6 == 1, 1, 0), 
      fin_activeloan_da_mm = ifelse(Q38_6 %in% c(97,99), NA, fin_activeloan_da_mm), 
      
      fin_activeloan_da_any = ifelse(fin_activeloan_da_cbnk == 1 | fin_activeloan_da_mfi == 1 | fin_activeloan_da_fintech == 1 | fin_activeloan_da_sacco == 1 | fin_activeloan_da_group == 1 | fin_activeloan_da_mm == 1, 1, 0), 
      fin_activeloan_da_any = ifelse(fin_activeloan_agg_any == 0, 0, fin_activeloan_da_any),
      
      # DId this business want a loan but could not obtain it?
      fin_deniedloan = ifelse(Q39 == 1, 1, 0),
      fin_deniedloan = ifelse(Q39 %in% c(97,99), NA, fin_deniedloan), 
      
      # Reason business could not obtain loan
      fin_deniedloan_reason_nosuit = ifelse(Q40 == 1, 1, 0),
      fin_deniedloan_reason_req = ifelse(Q40 == 2, 1, 0),
      fin_deniedloan_reason_cost = ifelse(Q40 == 3, 1, 0),
      fin_deniedloan_reason_terms = ifelse(Q40 == 4, 1, 0),
      fin_deniedloan_reason_denied = ifelse(Q40 %in% c(5,6), 1, 0),
      fin_deniedloan_reason_other= ifelse(Q40 == 7, 1, 0),
      
      # Use of active loans
      fin_activeloan_use_inv = ifelse(Q42_1 == 1, 1, 0),
      fin_activeloan_use_inv = ifelse(Q42_1 %in% c(97,99), NA, fin_activeloan_use_inv),
      fin_activeloan_use_exp = ifelse(Q42_2 == 1, 1, 0),
      fin_activeloan_use_exp = ifelse(Q42_2 %in% c(97,99), NA, fin_activeloan_use_exp),
      fin_activeloan_use_debt = ifelse(Q42_3 == 1, 1, 0),
      fin_activeloan_use_debt = ifelse(Q42_3 %in% c(97,99), NA, fin_activeloan_use_debt),
      fin_activeloan_use_exp = ifelse(Q42_4 == 1, 1, 0),
      fin_activeloan_use_exp = ifelse(Q42_4 %in% c(97,99), NA, fin_activeloan_use_exp),
      fin_activeloan_use_other = ifelse(Q42_5 == 1, 1, 0),
      fin_activeloan_use_other = ifelse(Q42_5 %in% c(97,99), NA, fin_activeloan_use_other), 
      
      # OVerall loan demand
      fin_demandloan = ifelse(fin_activeloan_agg_any == 1 | fin_deniedloan == 1, 1, 0), 
      
      # Supplier credit 
      fin_suppliercredit = ifelse(Q41 == 1, 1, 0), 
      fin_suppliercredit = ifelse(Q41 %in% c(97,99), NA, fin_suppliercredit), 
      
      # Trust of channels
      fin_channels_trust_physical = ifelse(Q43 == 1, 1, 0),
      fin_channels_trust_physical = ifelse(Q43 %in% c(97,99), NA, fin_channels_trust_physical),
      fin_channels_trust_digital = ifelse(Q43 == 2, 1, 0), 
      fin_channels_trust_digital = ifelse(Q43 %in% c(97,99), NA, fin_channels_trust_digital),
      fin_channels_trust_indiff = ifelse(Q43 == 3, 1, 0), 
      fin_channels_trust_indiff = ifelse(Q43 %in% c(97,99), NA, fin_channels_trust_indiff),
      
      # Benefits of digital financial services: 
      fin_digital_benefits_cus = ifelse(Q46_1 == 1, 1, 0), 
      fin_digital_benefits_rev = ifelse(Q46_2 == 1, 1, 0), 
      fin_digital_benefits_cst = ifelse(Q46_3 == 1, 1, 0), 
      fin_digital_benefits_tim = ifelse(Q46_4 == 1, 1, 0), 
      fin_digital_benefits_cf = ifelse(Q46_5 == 1, 1, 0), 
      fin_digital_benefits_bnk = ifelse(Q46_6 == 1, 1, 0), 
      fin_digital_benefits_crd = ifelse(Q46_7 == 1, 1, 0), 
      fin_digital_benefits_dat = ifelse(Q46_8 == 1, 1, 0), 
      fin_digital_benefits_none = ifelse(Q46_9 == 1, 1, 0), 
      
      # Benefits of digital financial services: 
      fin_digital_challenges_diff = ifelse(Q47_1 == 1, 1, 0), 
      fin_digital_challenges_cost = ifelse(Q47_2 == 1, 1, 0), 
      fin_digital_challenges_trust = ifelse(Q47_3 == 1, 1, 0), 
      fin_digital_challenges_rel = ifelse(Q47_4 == 1, 1, 0), 
      fin_digital_challenges_infra = ifelse(Q47_5 == 1, 1, 0), 
      fin_digital_challenges_other = ifelse(Q47_6 == 1, 1, 0), 
      fin_digital_challenges_none = ifelse(Q47_7 == 1, 1, 0), 
      
      # Open banking
      fin_openbanking_use = ifelse(Q48 == 1, 1, 0), 
      fin_openbanking_use = ifelse(Q48 %in% c(97,98), NA, fin_openbanking_use), 
      
      # Access strands
      
      fin_access_strand_sav_str = ifelse(fin_bus_savings_agg_fi == 1, "Institutional", NA), 
      fin_access_strand_sav_str = ifelse(fin_bus_savings_agg_fi == 0 & fin_bus_savings_agg_nbfi == 1, "Digital", fin_access_strand_sav_str), 
      fin_access_strand_sav_str = ifelse(fin_bus_savings_agg_fi == 0 & fin_bus_savings_agg_inf == 0 & fin_bus_savings_agg_inf == 1, "Informal", fin_access_strand_sav_str), 
      fin_access_strand_sav_str = ifelse(fin_bus_savings_agg_fi == 0 & fin_bus_savings_agg_inf == 0 & fin_bus_savings_agg_inf == 0, "Excluded", fin_access_strand_sav_str), 
      fin_access_strand_sav_str = ifelse(is.na(fin_access_strand_sav_str), "Excluded", fin_access_strand_sav_str), 
      
      fin_access_strand_sav_shc = case_when(
        fin_access_strand_sav_str == "Institutional" ~ "inst", 
        fin_access_strand_sav_str == "Digital" ~ "dig", 
        fin_access_strand_sav_str == "Informal" ~"inf", 
        fin_access_strand_sav_str == "Excluded" ~ "exc", 
      ), 
      
      fin_access_strand_loan_str = ifelse(fin_activeloan_agg_fi == 1, "Institutional", NA), 
      fin_access_strand_loan_str = ifelse(fin_activeloan_agg_fi == 0 & fin_activeloan_agg_nbfi == 1, "Digital", fin_access_strand_loan_str), 
      fin_access_strand_loan_str = ifelse(fin_activeloan_agg_fi == 0 & fin_activeloan_agg_nbfi == 0 & fin_activeloan_agg_inf == 1, "Informal", fin_access_strand_loan_str), 
      fin_access_strand_loan_str = ifelse(fin_activeloan_agg_fi == 0 & fin_activeloan_agg_nbfi == 0 & fin_activeloan_agg_inf == 0, "Excluded", fin_access_strand_loan_str), 
      fin_access_strand_loan_str = ifelse(is.na(fin_access_strand_loan_str), "Excluded", fin_access_strand_loan_str), 
      
      fin_access_strand_loan_shc = case_when(
        fin_access_strand_loan_str == "Institutional" ~ "inst", 
        fin_access_strand_loan_str == "Digital" ~ "dig", 
        fin_access_strand_loan_str == "Informal" ~"inf", 
        fin_access_strand_loan_str == "Excluded" ~ "exc", 
      ), 

      
       # Consumer proection risks---------
      
      # Loan repayment difficulty
      cp_loanrepaydiff = ifelse(Q49 == 1, 1, 0), 
      cp_loanrepaydiff = ifelse(Q49 %in% c(97, 99), NA, cp_loanrepaydiff), 
      cp_loanrepaydiff = ifelse(Q49 %in% c(98) | fin_activeloan_agg_any == 0, NA, cp_loanrepaydiff), 
      
      cp_loanrepaydiff_cope_notpay = ifelse(Q50_1 == 1, 1, 0), 
      cp_loanrepaydiff_cope_notpay = ifelse(Q50_1 %in% c(97, 99) | fin_activeloan_agg_any == 0, NA, cp_loanrepaydiff_cope_notpay), 
      cp_loanrepaydiff_cope_restr = ifelse(Q50_2 == 1, 1, 0), 
      cp_loanrepaydiff_cope_restr = ifelse(Q50_2 %in% c(97, 99)| fin_activeloan_agg_any == 0, NA, cp_loanrepaydiff_cope_restr),
      cp_loanrepaydiff_cope_borr = ifelse(Q50_3 == 1, 1, 0), 
      cp_loanrepaydiff_cope_borr = ifelse(Q50_3 %in% c(97, 99) | fin_activeloan_agg_any == 0, NA, cp_loanrepaydiff_cope_borr),
      cp_loanrepaydiff_cope_work = ifelse(Q50_4 == 1, 1, 0), 
      cp_loanrepaydiff_cope_work = ifelse(Q50_4 %in% c(97, 99) | fin_activeloan_agg_any == 0, NA, cp_loanrepaydiff_cope_work),
      cp_loanrepaydiff_cope_sellasst = ifelse(Q50_5 == 1, 1, 0), 
      cp_loanrepaydiff_cope_sellasst = ifelse(Q50_5 %in% c(97, 99)| fin_activeloan_agg_any == 0, NA, cp_loanrepaydiff_cope_sellasst),
      cp_loanrepaydiff_cope_usedhh = ifelse(Q50_6 == 1, 1, 0), 
      cp_loanrepaydiff_cope_usedhh = ifelse(Q50_6 %in% c(97, 99)| fin_activeloan_agg_any == 0, NA, cp_loanrepaydiff_cope_usedhh),
      cp_loanrepaydiff_cope_other = ifelse(Q50_7 == 1, 1, 0), 
      cp_loanrepaydiff_cope_other = ifelse(Q50_7 %in% c(97, 99) | fin_activeloan_agg_any == 0, NA, cp_loanrepaydiff_cope_other),
      
      # Issues experienced with financial services
      cp_issues_fraud = ifelse(Q51_1 == 1, 1, 0), 
      cp_issues_fraud = ifelse(Q51_1 == 3, NA, cp_issues_fraud), 
      cp_issues_salesp = ifelse(Q51_2 == 1, 1, 0), 
      cp_issues_salesp = ifelse(Q51_2 == 3, NA, cp_issues_salesp), 
      cp_issues_badtreat = ifelse(Q51_3 == 1, 1, 0), 
      cp_issues_badtreat = ifelse(Q51_3 == 3, NA, cp_issues_badtreat), 
      cp_issues_unxfees = ifelse(Q51_4 == 1, 1, 0),
      cp_issues_unxfees = ifelse(Q51_4 == 3, NA, cp_issues_unxfees), 
      cp_issues_crb = ifelse(Q51_5 == 1, 1, 0),
      cp_issues_crb = ifelse(Q51_5 == 3, NA, cp_issues_crb), 
      cp_issues_trms = ifelse(Q51_6 == 1, 1, 0),
      cp_issues_trms = ifelse(Q51_6 == 3, NA, cp_issues_trms),
      cp_issues_other = ifelse(Q51_7 ==1, 1, 0),
      cp_issues_other = ifelse(Q51_7 == 3, NA, cp_issues_other),
      
      cp_issues_any = ifelse(Q51_1 == 1 | Q51_2 == 1 | Q51_3 == 1 | Q51_4 == 1 | Q51_5 == 1 | Q51_6 == 1 | Q51_7 == 1, 1, 0), 
      
      cp_complaint = ifelse(Q52 ==1, 1, 0),
      
      cp_complaint_no_diff = ifelse(Q53 == 1, 1, 0), 
      cp_complaint_no_diff = ifelse(Q53 %in% c(97,99), NA, cp_complaint_no_diff), 
      cp_complaint_no_knowhow = ifelse(Q53 %in% c(2, 5), 1, 0), 
      cp_complaint_no_knowhow = ifelse(Q53 %in% c(97,99), NA, cp_complaint_no_knowhow), 
      cp_complaint_no_channel = ifelse(Q53 == 3, 1, 0), 
      cp_complaint_no_channel = ifelse(Q53 %in% c(97,99), NA, cp_complaint_no_channel), 
      cp_complaint_no_faith = ifelse(Q53 == 4, 1, 0), 
      cp_complaint_no_faith = ifelse(Q53 %in% c(97,99), NA, cp_complaint_no_faith), 
      
      cp_complaint_resolved_yes = ifelse(Q54 == 1, 1, 0), 
      cp_complaint_resolved_yes = ifelse(Q54 %in% c(97, 98, 99), NA, cp_complaint_resolved_yes),
      
      # Stopped using DFS due to fear of risks?
      cp_dfs_stopped = ifelse(Q55 == 1, 1, 0), 
      cp_dfs_stopped = ifelse(Q55 %in% c(97, 99), 1, 0), 
      
      # Risks & Resilience -------- 
      
      # In the last 36 months (3 years), which of the following risks had the largest impact (most costly) in terms of losses or expenses incurred by the business
      
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
      risk_weather_cope_noth = ifelse(is.na(risk_weather_cope_noth) & risk_weather_type_any == 1, 0, risk_weather_cope_noth),
      risk_weather_cope_othr = ifelse(Q59_12 == 1, 1, ifelse(Q59_12 == 2, 0, NA)),
      
      risk_weather_cond_ntrc = ifelse(Q60 == 1, 1, 0), 
      risk_weather_cond_ntrc = ifelse(Q60 %in% c(97,99), NA, risk_weather_cond_ntrc), 
      risk_weather_cond_rc = ifelse(Q60 == 2, 1, 0), 
      risk_weather_cond_rc = ifelse(Q60 %in% c(97,99), NA, risk_weather_cond_rc), 
      risk_weather_cond_rcbt = ifelse(Q60 == 3, 1, 0), 
      risk_weather_cond_rcbt = ifelse(Q60 %in% c(97,99), NA, risk_weather_cond_rcbt), 
      
      risk_weather_adaptspend = ifelse(Q61 == 1, 1, 0), 
      risk_weather_adaptspend = ifelse(Q61 %in% c(97,99), NA, risk_weather_adaptspend), 
      
      # Financial resiliences
      resi_efunds_firm_diff_3 = ifelse(Q62 == 1, 1, ifelse(Q62 %in% c(97,99), NA, 0)), 
      resi_efunds_firm_diff_2 = ifelse(Q62 == 2, 1, ifelse(Q62 %in% c(97,99), NA, 0)),
      resi_efunds_firm_diff_1 = ifelse(Q62 == 3, 1, ifelse(Q62 %in% c(97,99), NA, 0)), 
      resi_efunds_firm_diff_4 = ifelse(Q62 == 4, 1, ifelse(Q62 %in% c(97,99), NA, 0)), 

      resi_efunds_firm_str = case_when(
        resi_efunds_firm_diff_3 == 1 ~ "Very difficult", 
        resi_efunds_firm_diff_2 == 1 | resi_efunds_firm_diff_1 == 1 ~ "Somewhat or not difficult at all", 
        resi_efunds_firm_diff_4 == 1 ~ "Not possible"
      ), 
      
      resi_efunds_firm_score = case_when(
        resi_efunds_firm_diff_3 == 1 ~ 2, 
        resi_efunds_firm_diff_2 == 1 | resi_efunds_firm_diff_1 == 1 ~ 1, 
        resi_efunds_firm_diff_4 == 1 ~ 0
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
      resi_efunds_hh_owner_yes = ifelse(resp_type_owner == 0, 0, resi_efunds_hh_owner_yes), 
      resi_efunds_hh_owner_no = ifelse(Q64 == 2, 1, ifelse(Q64 %in% c(97,99), NA, 0)), 
      resi_efunds_hh_owner_no = ifelse(resp_type_owner == 0, 0, resi_efunds_hh_owner_no), 
      
      resi_efunds_hh_owner_score = ifelse(resi_efunds_hh_owner_no == 1, 1, 0), 
      
      resi_suppchain_cust_diff_1 = ifelse(Q65 == 1, 1, ifelse(Q65 %in% c(97,99), NA, 0)), 
      resi_suppchain_cust_diff_2 = ifelse(Q65 %in% c(2, 3), 1, ifelse(Q65 %in% c(97,99), NA, 0)), 
      resi_suppchain_cust_diff_3 = ifelse(Q65 == 4, 1, ifelse(Q65 %in% c(97,99), NA, 0)), 
      
      resi_suppchain_score = case_when(
        resi_suppchain_cust_diff_1 == 1 ~ 2, 
        resi_suppchain_cust_diff_2 == 1 ~ 1, 
        resi_suppchain_cust_diff_3 == 1 ~ 0
      ), 
      
      resi_resp_attitude_chllng_str = case_when(
        Q66 == 1 ~ "Felt overwhelmed and feared closure", 
        Q66 == 2 ~ "Viewed challenges as a normal aspect of business", 
        Q66 == 3 ~ "Sought new opportunities to expand or pivot", 
        Q66 == 4 ~ "I didn't face any significant challenges"
      ), 
      resi_resp_attitude_chllng_str = ifelse(Q66 %in% c(97,99), NA, resi_resp_attitude_chllng_str), 
      
      resi_resp_attitude_chllng_1 = ifelse(Q66 == 1, 1, ifelse(Q66 %in% c(97,99), NA, 0)), 
      resi_resp_attitude_chllng_2 = ifelse(Q66 == 2, 1, ifelse(Q66 %in% c(97,99), NA, 0)), 
      resi_resp_attitude_chllng_3 = ifelse(Q66 == 3, 1, ifelse(Q66 %in% c(97,99), NA, 0)), 
      resi_resp_attitude_chllng_4 = ifelse(Q66 == 4, 1, ifelse(Q66 %in% c(97,99), NA, 0)), 
      
      resi_network_conf_3 = ifelse(Q67 %in% c(1, 2), 1, ifelse(Q67 %in% c(97,99), NA, 0)), 
      resi_network_conf_2 = ifelse(Q67 %in% c(3), 1, ifelse(Q67 %in% c(97,99), NA, 0)), 
      resi_network_conf_1 = ifelse(Q67 == 4, 1, ifelse(Q67 %in% c(97,99), NA, 0)),
      
      resi_network_conf_score = case_when(
        resi_network_conf_1 == 1 ~ 2, 
        resi_network_conf_2 == 1 ~ 1, 
        resi_network_conf_3 == 1 ~ 0
      ), 
      
      # Resilience capital
      # Financial: Can raise emergency funds in 7 days without major difficulty
      resi_capital_financial = ifelse(resi_efunds_firm_str == "Somewhat or not difficult at all", 1, 0), 
      # Dependency on customers: Can overcome challenges with main customer or supplier without major difficulty
      resi_capital_linkages = ifelse(resi_suppchain_cust_diff_1 == 1 | resi_suppchain_cust_diff_2 == 1, 1, 0), 
      # Dependency on household: Business savings not vulnerable to household shocks
      resi_capital_hh = ifelse(resi_efunds_hh_owner_no == 1, 1, 0), 
      # Confidence in network 
      resi_capital_network = ifelse(resi_network_conf_2 == 1 | resi_network_conf_1 == 1, 1, 0), 
      
      resi_capital_score_v1 = resi_capital_financial + resi_capital_linkages + resi_capital_hh + resi_capital_network, 
      resi_capital_score_v2 = resi_capital_financial + resi_capital_linkages + resi_capital_network, 
      resi_capital_score_v3 = resi_efunds_firm_score + resi_efunds_hh_owner_score + resi_suppchain_score + resi_network_conf_score, 
      resi_capital_score_v3_norm = resi_capital_score_v3/7
      
    ) %>% 
    dummy_cols(select_columns = c("business_premise_shc", "business_size_agg2_shc", "business_sector_agg2_shc", "business_sector_agg3_shc",
                                  "resp_experience_agg5_shc", "resp_education_agg2_shc", "resp_education_agg4_shc", "resp_education_agg5_shc", "resp_age_agg3_shc", "resp_age_agg6_shc", "resp_psych_segment_shc", 
                                  "tech_uses_messaging_shc", "tech_uses_socialmedia_shc", "tech_uses_ecommerce_shc", "tech_uses_software_shc", "fin_access_strand_sav_shc", "fin_access_strand_loan_shc", 
                                  "risk_largestimpact_shc")) -> raw_data 
  
    # Insurance ----
    
    if (selected_country %in% c("Brazil", "Indonesia")) {
      
      raw_data %>% mutate(
  
        fin_insur_lif_shc = case_when(
          B1_1 == 1 ~ "cu", #Currently use
          B1_1 == 2 ~ "su", # Stopped using
          B1_1 == 3 ~ "nh", # Never had
          B1_1 == 4 ~ "na" # Not aware
        ), 
        fin_insur_hlt_shc = case_when(
          B1_2 == 1 ~ "cu", #Currently use
          B1_2 == 2 ~ "su", # Stopped using
          B1_2 == 3 ~ "nh", # Never had
          B1_2 == 4 ~ "na" # Not aware
        ), 
        fin_insur_acc_shc = case_when(
          B1_3 == 1 ~ "cu", #Currently use
          B1_3 == 2 ~ "su", # Stopped using
          B1_3 == 3 ~ "nh", # Never had
          B1_3 == 4 ~ "na" # Not aware
        ), 
        fin_insur_fun_shc = case_when(
          B1_4 == 1 ~ "cu", #Currently use
          B1_4 == 2 ~ "su", # Stopped using
          B1_4 == 3 ~ "nh", # Never had
          B1_4 == 4 ~ "na" # Not aware
        ),       
        fin_insur_hom_shc = case_when(
          B1_5 == 1 ~ "cu", #Currently use
          B1_5 == 2 ~ "su", # Stopped using
          B1_5 == 3 ~ "nh", # Never had
          B1_5 == 4 ~ "na" # Not aware
        ), 
        fin_insur_bus_shc = case_when(
          B1_6 == 1 ~ "cu", #Currently use
          B1_6 == 2 ~ "su", # Stopped using
          B1_6 == 3 ~ "nh", # Never had
          B1_6 == 4 ~ "na" # Not aware
        ), 
        fin_insur_aut_shc = case_when(
          B1_7== 1 ~ "cu", #Currently use
          B1_7 == 2 ~ "su", # Stopped using
          B1_7 == 3 ~ "nh", # Never had
          B1_7 == 4 ~ "na" # Not aware
        ), 
        fin_insur_idx_shc = case_when(
          B1_8== 1 ~ "cu", #Currently use
          B1_8 == 2 ~ "su", # Stopped using
          B1_8 == 3 ~ "nh", # Never had
          B1_8 == 4 ~ "na" # Not aware
        ), 
        fin_insur_oth_shc = case_when(
          B1_9 == 1 ~ "cu", #Currently use
          B1_9 == 2 ~ "su", # Stopped using
          B1_9 == 3 ~ "nh", # Never had
          B1_9 == 4 ~ "na" # Not aware
        ), 
        
      # Digital access to insurance
      fin_insur_lif_da = ifelse(B2_1 == 1, 1, ifelse(B2_1 == 2, 0, NA)), 
      fin_insur_hlt_da = ifelse(B2_2 == 1, 1, ifelse(B2_2 == 2, 0, NA)), 
      fin_insur_acc_da = ifelse(B2_3 == 1, 1, ifelse(B2_3 == 2, 0, NA)), 
      fin_insur_fun_da = ifelse(B2_4 == 1, 1, ifelse(B2_4 == 2, 0, NA)), 
      fin_insur_hom_da = ifelse(B2_5 == 1, 1, ifelse(B2_5 == 2, 0, NA)), 
      fin_insur_bus_da = ifelse(B2_6 == 1, 1, ifelse(B2_6 == 2, 0, NA)), 
      fin_insur_aut_da = ifelse(B2_7 == 1, 1, ifelse(B2_7 == 2, 0, NA)), 
      fin_insur_idx_da = ifelse(B2_8 == 1, 1, ifelse(B2_8 == 2, 0, NA)), 
      fin_insur_oth_da = ifelse(B2_9 == 1, 1, ifelse(B2_9 == 2, 0, NA)), 
      
      fin_insur_hltlif_da = ifelse(fin_insur_lif_da == 1 | fin_insur_hlt_da == 1, 1, 0), 
      fin_insur_any_da = ifelse(fin_insur_lif_da == 1 | fin_insur_hlt_da == 1 | fin_insur_acc_da == 1 | fin_insur_fun_da == 1 | fin_insur_hom_da == 1 | fin_insur_bus_da == 1 | fin_insur_aut_da == 1 | fin_insur_idx_da == 1 | fin_insur_oth_da == 1, 1, 0), 
      
      # Life or health insurance cover for specific family member
      
      fin_insur_lifhlt_slf = ifelse(B3_O1 == 1 | B3_O2 == 1 | B3_O3 == 1 | B3_O4 == 1 | B3_O5 == 1 | B3_O6 == 1 | B3_O7 == 1 | B3_O8 == 1, 1, 0), 
      fin_insur_lifhlt_spo = ifelse(B3_O1 == 2 | B3_O2 == 2 | B3_O3 == 2 | B3_O4 == 2 | B3_O5 == 2 | B3_O6 == 2 | B3_O7 == 2 | B3_O8 == 2, 1, 0), 
      fin_insur_lifhlt_chi = ifelse(B3_O1 == 3 | B3_O2 == 3 | B3_O3 == 3 | B3_O4 == 3 | B3_O5 == 3 | B3_O6 == 3 | B3_O7 == 3 | B3_O8 == 3, 1, 0), 
      fin_insur_lifhlt_par = ifelse(B3_O1 == 4 | B3_O2 == 4 | B3_O3 == 4 | B3_O4 == 4 | B3_O5 == 4 | B3_O6 == 5 | B3_O7 == 4 | B3_O8 == 4, 1, 0), 
      fin_insur_lifhlt_oth = ifelse(B3_O1 == 5 | B3_O2 == 5 | B3_O3 == 5 | B3_O4 == 5 | B3_O5 == 5 | B3_O6 == 5 | B3_O7 == 5 | B3_O8 == 5, 1, 0), 
      
      # Types of health insurance used
      
      fin_insur_hlt_op = ifelse(A1_1 == 1, 1, ifelse(A1_1 == 2, 0, NA)), 
      fin_insur_hlt_ip = ifelse(A1_2 == 1, 1, ifelse(A1_2 == 2, 0, NA)), 
      fin_insur_hlt_sr = ifelse(A1_3 == 1, 1, ifelse(A1_3 == 2, 0, NA)),  
      fin_insur_hlt_vi = ifelse(A1_4 == 1, 1, ifelse(A1_4 == 2, 0, NA)),   
      fin_insur_hlt_dn = ifelse(A1_5 == 1, 1, ifelse(A1_5 == 2, 0, NA)),  
      fin_insur_hlt_ca = ifelse(A1_6 == 1, 1, ifelse(A1_6 == 2, 0, NA)), 
      fin_insur_hlt_di = ifelse(A1_7 == 1, 1, ifelse(A1_7 == 2, 0, NA)), 
      fin_insur_hlt_em = ifelse(A1_8 == 1, 1, ifelse(A1_8 == 2, 0, NA)), 
      fin_insur_hlt_px = ifelse(A1_9 == 1, 1, ifelse(A1_9 == 2, 0, NA)), 
      fin_insur_hlt_tm = ifelse(A1_10 == 1, 1, ifelse(A1_10 == 2, 0, NA)), 
      fin_insur_hlt_ot = ifelse(A1_11 == 1, 1, ifelse(A1_11 == 2, 0, NA)), 
      
      # Business with employees offers health insurance benefit to employees? 
      
      fin_insur_lifhlt_empbft = ifelse(A2 == 1, 1, 0), 
      fin_insur_lifhlt_empbft = ifelse(A2 %in% c(3, 4), NA, fin_insur_lifhlt_empbft), 
      
      fin_insur_lifhlt_empbft_yes = ifelse(A2 == 1, 1, 0), 
      fin_insur_lifhlt_empbft_yes = ifelse(A2 %in% c(4), NA, fin_insur_lifhlt_empbft_yes), 
      fin_insur_lifhlt_empbft_no = ifelse(A2 == 2, 1, 0), 
      fin_insur_lifhlt_empbft_no = ifelse(A2 %in% c(4), NA, fin_insur_lifhlt_empbft_no), 
      fin_insur_lifhlt_empbft_noemp = ifelse(A2 == 3, 1, 0), 
      fin_insur_lifhlt_empbft_noemp = ifelse(A2 %in% c(4), NA, fin_insur_lifhlt_empbft_noemp), 
      
      # Climate risks 
      
      # In Indonesia only 1 respondent said they used insurance payouts in response to a climate shock, in Brazil 3 respondents
      
      # C7. In light of the climate risk, how easy or difficult was it to access your insurance claim payout?
      # If respondents responded yes to Q59 option 6 (Used insurance payouts)
      
      # C8. How quickly could you access your insurance claim payout?
      # For respondents who selected options 1,2, 3 or 4 in C7. If not skip to C12
      
      # C9. What was the primary use of the insurance claim payout? 
      # For respondents who selected options 1,2, 3 or 4 in C7. If not skip to C12
      
      # C10. To what extent did the insurance payout cover the losses or damages experienced?
      # For respondents who selected options 1,2, 3 or 4 in C7. If not skip to C12
      
      # C11. Based on your experience, how likely are you to recommend  insurance to other entrepreneurs to help them to deal with climate-related risks?
      # For respondents who selected options 1,2, 3 or 4 in C7. If not skip to C12
      
      # C12. In the next 12 months how likely are you to purchase insurance, or expand your insurance coverage to protect against climate risks?
      # Ask all 
      
      risk_cli_inspur_num = ifelse(C12 %in% c(6, 7), NA, C12), 
      
      risk_cli_inspur_shc = case_when(
        C12 %in% c(1,2) ~ "lik", 
        C12 %in% c(3) ~ "neu", 
        C12 %in% c(4,5) ~ "unlik", 
        C12 %in% c(6, 7) ~ NA
      ), 
      
      # Health risks 
      
      # H1. In the last 36 months (3 years), did the owner, manager, or any employee have health issue that impacted your business operations? 
      # Ask all
      risk_hlt_exp = ifelse(H1 == 1, 1, 0), 
      risk_hlt_exp = ifelse(H1 %in% c(97, 99), NA, risk_hlt_exp), 
      
      risk_hlt_exp_str = ifelse(risk_hlt_exp == 1, "Experienced health shock (in past 36 mos)", "Did not experience health shock (in past 36 mos)"), 
      
      # Faced a climate or health shock that impacted business in past 36 months: 
      
      risk_hltcli_exp = ifelse(risk_hlt_exp == 1 | risk_weather_type_any == 1, 1, 0), 
        
      # H2 Please describe how the most significant health issue(s) in the last 36 months impacted your business operations?
      # IF H! == Yes, 
      
      risk_hlt_imp_fin = ifelse(H2_1 == 1, 1, 0), # Decreased sales, income or profits
      risk_hlt_imp_ops = ifelse(H2_2 == 1, 1, 0), #Unable to operate the business as usual
      risk_hlt_imp_prod = ifelse(H2_3 == 1, 1, 0), # Reduced productivity
      risk_hlt_imp_opcosts = ifelse(H2_4 == 1, 1, 0), # Increased operational costs
      risk_hlt_imp_oth = ifelse(H2_5 == 1, 1, 0), 
      
     # H3. What actions did the business take to cope with the impacts of this health issue? 
     
     risk_hlt_cope_borrfin = ifelse(H3_1 == 1, 1, 0), 
     risk_hlt_cope_borrinf = ifelse(H3_2 == 1, 1, 0), 
     risk_hlt_cope_savbus = ifelse(H3_3 == 1, 1, 0), 
     risk_hlt_cope_ins = ifelse(H3_6 == 1, 1, 0), 
     risk_hlt_cope_hh = ifelse(H3_4 == 1, 1, 0), 
     risk_hlt_cope_ff = ifelse(H3_7 == 1, 1, 0), 
     risk_hlt_cope_oth = ifelse(H3_5 == 1 | H3_8 == 1 | H3_9 == 1 | H3_10 == 1, 1, 0), 
     risk_hlt_cope_noth = ifelse(H3_11 == 1, 1, 0), 
     
     # H4. What is the condition of the business now, compared to before the health risk occurred? 
     
     risk_hlt_cond_notrec = ifelse(H4 == 1, 1, 0), 
     risk_hlt_cond_rec = ifelse(H4 == 2, 1, 0), 
     risk_hlt_cond_fullrec = ifelse(H4 == 3, 1, 0), 
     
     # H5. Approximately, how many months did it take the business to recover to its pre-event condition?
     
     risk_hlt_rec_lt6mos = ifelse(H5 %in% c(1, 2), 1, 0), 
     risk_hlt_rec_mt6mos = ifelse(H5 %in% c(3, 4), 1, 0),
     risk_hlt_rec_dk = ifelse(H5 %in% c(97), 1, 0),
     
     risk_hlt_rec_lt6mos = ifelse(risk_hlt_cond_rec == 1 | risk_hlt_cond_fullrec == 1, risk_hlt_rec_lt6mos, NA), 
     risk_hlt_rec_mt6mos = ifelse(risk_hlt_cond_rec == 1 | risk_hlt_cond_fullrec == 1, risk_hlt_rec_mt6mos, NA), 
     risk_hlt_rec_dk = ifelse(risk_hlt_cond_rec == 1 | risk_hlt_cond_fullrec == 1, risk_hlt_rec_dk, NA), 
     
    # H6.  How easy or difficult was it to access your insurance claim payout?
    # For respondents who said yes to option 6 question H3 (i.e., they accessed money from insurance). If not, ask H11
    
    risk_hlt_claimdiff_shc = case_when(
      H6 %in% c(1,2) ~ "notdiff", 
      H6 %in% c(3) ~ "some", 
      H6 %in% c(4) ~ "very", 
      H6 %in% c(5) ~ "not", 
      H6 %in% c(6, 7) ~ NA
    ), 
    
    # H7. How quickly could you access your insurance claim funds? 
    # For respondents who said yes to option 6 question H3 (i.e., they accessed money from insurance). If not, ask H11
    
    risk_hlt_claimquick_shc = case_when(
      H6 %in% c(1,2) ~ "qck", 
      H6 %in% c(3, 4, 5) ~ "slw", 
      H6 %in% c(6) ~ "not", 
      H6 %in% c(7, 8) ~ NA
    ), 
    
    # H8. What was the primary use of the insurance claim payout?  
    # For respondents who said yes to option 6 question H3 (i.e., they accessed money from insurance). If not, ask H11
    
    risk_hlt_claimuse_shc = case_when(
      H8 %in% c(1) ~ "pmb", 
      H8 %in% c(2) ~ "cov", 
      H8 %in% c(3) ~ "dbt", 
      H8 %in% c(4) ~ "oth"
    ), 
    
    # To what extent did the insurance payout cover the health expenses experienced?
    
    risk_hlt_payout_full = ifelse(H9 %in% c(1,2), 1, 0), 
    risk_hlt_payout_partial = ifelse(H9 == 3, 1, 0), 
    risk_hlt_payout_none = ifelse(H9 == 4, 1, 0), 
    risk_hlt_payout_dk = ifelse(H9 %in% c(5,6), 1, 0), 
    
    # H10: Based on your experience, how likely are you to recommend  insurance to other entrepreneurs to help them to deal with health-related risks?
    
    risk_hlt_insrec0_num = ifelse(H10 %in% c(6, 7), NA, H11), 
    
    risk_hlt_insreco_shc = case_when(
      H10 %in% c(1,2) ~ "lik", 
      H10 %in% c(3) ~ "neu", 
      H10 %in% c(4,5) ~ "unlik", 
      H10 %in% c(6, 7) ~ NA
    ), 
    
    # H11. In the next 12 months, how likely are you to purchase insurance, or expand your insurance coverage to protect against health risks ?
    
    risk_hlt_inspur_num = ifelse(H11 %in% c(6, 7), NA, H11), 
    
    risk_hlt_inspur_shc = case_when(
      H11 %in% c(1,2) ~ "lik", 
      H11 %in% c(3) ~ "neu", 
      H11 %in% c(4,5) ~ "unlik", 
      H11 %in% c(6, 7) ~ NA
    ), 
    
    # Please select the channels(s) through which you purchased your insurance policy. 
    # For respondents who said currently have or had it in the past to options 1 to 9 of question B1, or if yes option 6 question H3, or if yes to option 6 question 59
    
    fin_insur_chnnl_direct = ifelse(I1_1 == 1, 1, 0), 
    fin_insur_chnnl_agent = ifelse(I1_2 == 1, 1, 0), 
    fin_insur_chnnl_online = ifelse(I1_3 == 1, 1, 0),   
    fin_insur_chnnl_fi = ifelse(I1_4 == 1, 1, 0), 
    fin_insur_chnnl_grp = ifelse(I1_5 == 1, 1, 0), 
    fin_insur_chnnl_ecom = ifelse(I1_6 == 1, 1, 0), 
    fin_insur_chnnl_bun = ifelse(I1_7 == 1, 1, 0), 
    fin_insur_chnnl_gov = ifelse(I1_8 == 1, 1, 0), 
    fin_insur_chnnl_oth = ifelse(I1_9 == 1, 1, 0), 
    
    # Did you purchase your insurance policy online? 
    
    fin_insur_onlinetype_mkt = ifelse(I2 == 1, 1, 0), 
    fin_insur_onlinetype_web = ifelse(I2 == 2, 1, 0),
    fin_insur_onlinetype_app = ifelse(I2 == 3, 1, 0),
    fin_insur_onlinetype_oth = ifelse(I2 == 4, 1, 0),
    
    # In general, do you feel your overall insurance coverage meets all your needs? 
    #For respondents who said currently have or had it in the past to options 1 to 9 of question B1, or if yes option 6 question H3, or if yes to option 6 question 59
    
    fin_insur_needs_yes = ifelse(I3 == 1, 1, 0), 
    fin_insur_needs_som = ifelse(I3 == 2, 1, 0), 
    fin_insur_needs_no = ifelse(I3 == 3, 1, 0), 
    
    # Factors for purchasing insurance
    # For respondents who selected 'currently have insurance' for any option in B1
    
    fin_insur_factors_top_prm = ifelse(I4_O1 == 1 | I4_O2 == 1 | I4_O3 == 1, 1, 0), 
    fin_insur_factors_top_cov = ifelse(I4_O1 == 2 | I4_O2 == 2 | I4_O3 == 2, 1, 0), 
    fin_insur_factors_top_dig = ifelse(I4_O1 %in% c(3,4) | I4_O2 %in% c(3,4) | I4_O3 %in% c(3,4), 1, 0), 
    fin_insur_factors_top_srv = ifelse(I4_O1 %in% c(5,6,7)  | I4_O2 %in% c(5,6,7)  | I4_O3 %in% c(5,6,7), 1, 0), 
    fin_insur_factors_top_mnd = ifelse(I4_O1 %in% c(8,9) | I4_O2 %in% c(8,9) | I4_O3 %in% c(8,9), 1, 0), 
    
    # Considerations considered for expanding coverage 
    #For respondents who selected 'currently have insurance' for any option in B1
    
    fin_insur_exp_cov = ifelse(I5 == 1, 1, 0), 
    fin_insur_exp_rsk = ifelse(I5 == 2, 1, 0), 
    fin_insur_exp_ded = ifelse(I5 == 3, 1, 0), 
    fin_insur_exp_mem = ifelse(I5 == 4, 1, 0), 
    fin_insur_exp_oth = ifelse(I5 == 5, 1, 0), 
    
    #  Have you ever filed any insurance claims?
    # For respondents who selected 'currently have insurance' or 'had insurance in the past, but not in the last 12 months' for any option in B1
    
    fin_insur_claim_yes = ifelse(I6 == 1, 1, 0), 
    
    # IF yes, what was the outcome
    # For respondents who selected options 1 in question I6
    fin_insur_claim_out_rei = ifelse(I7 == 1, 1, 0),
    fin_insur_claim_out_par = ifelse(I7 == 2, 1, 0),
    fin_insur_claim_out_rej = ifelse(I7 == 3, 1, 0),
    fin_insur_claim_out_wai = ifelse(I7 == 4, 1, 0),
    
    # How would you rate your satisfaction with the insurance claims process?
    # For respondents who selected options 1 or 2 in question  I7
    claim_refpop = ifelse(I7 %in% c(1, 2), 1, 0), 
    
    fin_insur_claim_sat_hi = ifelse(I8 %in% c(1,2), 1, 0), 
    fin_insur_claim_sat_hi = ifelse(claim_refpop == 1, fin_insur_claim_sat_hi, NA), 
    fin_insur_claim_sat_neu = ifelse(I8 %in% c(3), 1, 0), 
    fin_insur_claim_sat_neu = ifelse(claim_refpop == 1, fin_insur_claim_sat_neu, NA), 
    fin_insur_claim_sat_lo = ifelse(I8 %in% c(4,5), 1, 0),
    fin_insur_claim_sat_lo = ifelse(claim_refpop == 1, fin_insur_claim_sat_lo, NA), 
    
    # What benefits have you experienced from having insurance?
    # For respondents who selected 'currently have insurance' or 'had insurance in the past, but not in the last 12 months' for any option in B1
    
    fin_insur_benef_sec = ifelse(I9_1 == 1 | I9_4 == 1, 1, 0), 
    fin_insur_benef_rsk = ifelse(I9_2 == 1, 1, 0), 
    fin_insur_benef_crd = ifelse(I9_3 == 1, 1, 0), 
    fin_insur_benef_oth = ifelse(I9_5 ==1 | I9_7 == 1, 1, 0), 
    fin_insur_benef_non = ifelse(I9_6 == 1, 1, 0), 
    
    # Would you recommend insurance to others?
    # For respondents who selected 'currently have insurance' or 'had insurance in the past, but not in the last 12 months' for any option in B1
    
    fin_insur_rec_lif_yes = ifelse(I10_1 == 1, 1, 0), 
    fin_insur_rec_lif_myb = ifelse(I10_1 == 2, 1, 0), 
    fin_insur_rec_lif_no = ifelse(I10_1 == 3, 1, 0),
    fin_insur_rec_lif_dk = ifelse(I10_1 %in% c(4,5), 1, 0),
    
    fin_insur_rec_hlt_yes = ifelse(I10_2 == 1, 1, 0), 
    fin_insur_rec_hlt_myb = ifelse(I10_2 == 2, 1, 0), 
    fin_insur_rec_hlt_no = ifelse(I10_2 == 3, 1, 0), 
    fin_insur_rec_hlt_dk = ifelse(I10_2 %in% c(4,5), 1, 0), 
    
    fin_insur_rec_acc_yes = ifelse(I10_3 == 1, 1, 0), 
    fin_insur_rec_acc_myb = ifelse(I10_3 == 2, 1, 0), 
    fin_insur_rec_acc_no = ifelse(I10_3 == 3, 1, 0), 
    fin_insur_rec_acc_dk = ifelse(I10_3 %in% c(4,5), 1, 0), 
    
    fin_insur_rec_fun_yes = ifelse(I10_4 == 1, 1, 0), 
    fin_insur_rec_fun_myb = ifelse(I10_4 == 2, 1, 0), 
    fin_insur_rec_fun_no = ifelse(I10_4 == 3, 1, 0), 
    fin_insur_rec_fun_dk = ifelse(I10_4 %in% c(4,5), 1, 0), 
    
    fin_insur_rec_hom_yes = ifelse(I10_5 == 1, 1, 0), 
    fin_insur_rec_hom_myb = ifelse(I10_5 == 2, 1, 0), 
    fin_insur_rec_hom_no = ifelse(I10_5 == 3, 1, 0), 
    fin_insur_rec_hom_dk = ifelse(I10_5 %in% c(4,5), 1, 0), 
    
    fin_insur_rec_bus_yes = ifelse(I10_6 == 1, 1, 0), 
    fin_insur_rec_bus_myb = ifelse(I10_6 == 2, 1, 0), 
    fin_insur_rec_bus_no = ifelse(I10_6 == 3, 1, 0), 
    fin_insur_rec_bus_dk = ifelse(I10_6 %in% c(4,5), 1, 0), 
    
    fin_insur_rec_aut_yes = ifelse(I10_7 == 1, 1, 0), 
    fin_insur_rec_aut_myb = ifelse(I10_7 == 2, 1, 0), 
    fin_insur_rec_aut_no = ifelse(I10_7 == 3, 1, 0), 
    fin_insur_rec_aut_dk = ifelse(I10_7 %in% c(4,5), 1, 0), 
    
    fin_insur_rec_idx_yes = ifelse(I10_8 == 1, 1, 0), 
    fin_insur_rec_idx_myb = ifelse(I10_8 == 2, 1, 0), 
    fin_insur_rec_idx_no = ifelse(I10_8 == 3, 1, 0), 
    fin_insur_rec_idx_dk = ifelse(I10_8 %in% c(4,5), 1, 0), 
    
    fin_insur_rec_oth_yes = ifelse(I10_9 == 1, 1, 0), 
    fin_insur_rec_oth_myb = ifelse(I10_9 == 2, 1, 0), 
    fin_insur_rec_oth_no = ifelse(I10_9 == 3, 1, 0), 
    fin_insur_rec_oth_dk = ifelse(I10_9 %in% c(4,5), 1, 0), 
    
    #If you currently do not have insurance or have discontinued your insurance, what is the primary reason for this decision? 
    # For respondents who selected either Had insurance in the past, but not in the last 12 months or Never had to all B1 options
    
    fin_insur_nonewhy_hiprm = ifelse(I11 == 1, 1, 0), 
    fin_insur_nonewhy_rigprm = ifelse(I11 == 2, 1, 0), 
    fin_insur_nonewhy_app = ifelse(I11 == 5, 1, 0), 
    fin_insur_nonewhy_noval = ifelse(I11 %in% c(3, 6, 7), 1, 0), 
    fin_insur_nonewhy_negex = ifelse(I11 == 4, 1, 0), 
    fin_insur_nonewhy_oth = ifelse(I11 == 8, 1, 0), 
    
    # Please rate your agreement with each statement on a scale from 1 to 5
    # All respondents
    
    fin_insur_statm_1_num = ifelse(I12_1 %in% c(97,99), NA, I12_1), 
    fin_insur_statm_2_num = ifelse(I12_2 %in% c(97,99), NA, I12_2), 
    fin_insur_statm_3_num = ifelse(I12_3 %in% c(97,99), NA, I12_3), 
    fin_insur_statm_4_num = ifelse(I12_4 %in% c(97,99), NA, I12_4), 
    fin_insur_statm_5_num = ifelse(I12_5 %in% c(97,99), NA, I12_5), 
    fin_insur_statm_6_num = ifelse(I12_6 %in% c(97,99), NA, I12_6), 
    fin_insur_statm_7_num = ifelse(I12_7 %in% c(97,99), NA, I12_7), 
    fin_insur_statm_8_num = ifelse(I12_8 %in% c(97,99), NA, I12_8), 
    fin_insur_statm_9_num = ifelse(I12_9 %in% c(97,99), NA, I12_9), 
    
    fin_insur_statm_1_sd = ifelse(I12_1 == 1, 1, 0), 
    fin_insur_statm_1_d = ifelse(I12_1 == 2, 1, 0), 
    fin_insur_statm_1_n = ifelse(I12_1 == 3, 1, 0), 
    fin_insur_statm_1_a = ifelse(I12_1 == 4, 1, 0), 
    fin_insur_statm_1_sa = ifelse(I12_1 == 5, 1, 0), 
    fin_insur_statm_1_dk = ifelse(I12_1 %in% c(97,99), 1, 0), 
    
    fin_insur_statm_2_sd = ifelse(I12_2 == 1, 1, 0), 
    fin_insur_statm_2_d = ifelse(I12_2 == 2, 1, 0), 
    fin_insur_statm_2_n = ifelse(I12_2 == 3, 1, 0), 
    fin_insur_statm_2_a = ifelse(I12_2 == 4, 1, 0), 
    fin_insur_statm_2_sa = ifelse(I12_2 == 5, 1, 0), 
    fin_insur_statm_2_dk = ifelse(I12_2 %in% c(97,99), 1, 0), 
    
    fin_insur_statm_3_sd = ifelse(I12_3 == 1, 1, 0), 
    fin_insur_statm_3_d = ifelse(I12_3 == 2, 1, 0), 
    fin_insur_statm_3_n = ifelse(I12_3 == 3, 1, 0), 
    fin_insur_statm_3_a = ifelse(I12_3 == 4, 1, 0), 
    fin_insur_statm_3_sa = ifelse(I12_3 == 5, 1, 0), 
    fin_insur_statm_3_dk = ifelse(I12_3 %in% c(97,99), 1, 0), 
    
    fin_insur_statm_4_sd = ifelse(I12_4 == 1, 1, 0), 
    fin_insur_statm_4_d = ifelse(I12_4 == 2, 1, 0), 
    fin_insur_statm_4_n = ifelse(I12_4 == 3, 1, 0), 
    fin_insur_statm_4_a = ifelse(I12_4 == 4, 1, 0), 
    fin_insur_statm_4_sa = ifelse(I12_4 == 5, 1, 0), 
    fin_insur_statm_4_dk = ifelse(I12_4 %in% c(97,99), 1, 0), 
    
    fin_insur_statm_5_sd = ifelse(I12_5 == 1, 1, 0), 
    fin_insur_statm_5_d = ifelse(I12_5 == 2, 1, 0), 
    fin_insur_statm_5_n = ifelse(I12_5 == 3, 1, 0), 
    fin_insur_statm_5_a = ifelse(I12_5 == 4, 1, 0), 
    fin_insur_statm_5_sa = ifelse(I12_5 == 5, 1, 0), 
    fin_insur_statm_5_dk = ifelse(I12_5 %in% c(97,99), 1, 0), 
    
    fin_insur_statm_6_sd = ifelse(I12_6 == 1, 1, 0), 
    fin_insur_statm_6_d = ifelse(I12_6 == 2, 1, 0), 
    fin_insur_statm_6_n = ifelse(I12_6 == 3, 1, 0), 
    fin_insur_statm_6_a = ifelse(I12_6 == 4, 1, 0), 
    fin_insur_statm_6_sa = ifelse(I12_6 == 5, 1, 0), 
    fin_insur_statm_6_dk = ifelse(I12_6 %in% c(97,99), 1, 0), 
    
    fin_insur_statm_7_sd = ifelse(I12_7 == 1, 1, 0), 
    fin_insur_statm_7_d = ifelse(I12_7 == 2, 1, 0), 
    fin_insur_statm_7_n = ifelse(I12_7 == 3, 1, 0), 
    fin_insur_statm_7_a = ifelse(I12_7 == 4, 1, 0), 
    fin_insur_statm_7_sa = ifelse(I12_7 == 5, 1, 0), 
    fin_insur_statm_7_dk = ifelse(I12_7 %in% c(97,99), 1, 0),
    
    fin_insur_statm_8_sd = ifelse(I12_8 == 1, 1, 0), 
    fin_insur_statm_8_d = ifelse(I12_8 == 2, 1, 0), 
    fin_insur_statm_8_n = ifelse(I12_8 == 3, 1, 0), 
    fin_insur_statm_8_a = ifelse(I12_8 == 4, 1, 0), 
    fin_insur_statm_8_sa = ifelse(I12_8 == 5, 1, 0), 
    fin_insur_statm_8_dk = ifelse(I12_8 %in% c(97,99), 1, 0),
    
    fin_insur_statm_9_sd = ifelse(I12_9 == 1, 1, 0), 
    fin_insur_statm_9_d = ifelse(I12_9 == 2, 1, 0), 
    fin_insur_statm_9_n = ifelse(I12_9 == 3, 1, 0), 
    fin_insur_statm_9_a = ifelse(I12_9 == 4, 1, 0), 
    fin_insur_statm_9_sa = ifelse(I12_9 == 5, 1, 0), 
    fin_insur_statm_9_dk = ifelse(I12_9 %in% c(97,99), 1, 0),
    
    # Over the next 12 months, which insurance are you likely to purchase? 
    # If the respondent is willing to buy insurance (If I12 Option 5 = 4 or 5)
    fin_insur_buy_lif = ifelse(I13_1 == 1, 1, 0), 
    fin_insur_buy_hlt = ifelse(I13_2 == 1, 1, 0), 
    fin_insur_buy_acc = ifelse(I13_3 == 1, 1, 0), 
    fin_insur_buy_fun = ifelse(I13_4 == 1, 1, 0), 
    fin_insur_buy_hom = ifelse(I13_5 == 1, 1, 0),  
    fin_insur_buy_bus = ifelse(I13_6 == 1, 1, 0),  
    fin_insur_buy_aut = ifelse(I13_7 == 1, 1, 0),  
    fin_insur_buy_idx = ifelse(I13_8 == 1, 1, 0),  
    fin_insur_buy_oth = ifelse(I13_9 == 1, 1, 0),  
    
    # How much did you spend on medical expenses for yourself and your family in the last 12 months? 
    # All respondents 
    
    fin_insur_medexp_0 = ifelse(I14 == 1, 1, 0), 
    fin_insur_medexp_1 = ifelse(I14 == 2, 1, 0),
    fin_insur_medexp_2 = ifelse(I14 == 3, 1, 0),
    fin_insur_medexp_3 = ifelse(I14 == 4, 1, 0), 
    fin_insur_medexp_dk = ifelse(I14 %in% c(5,6), 1, 0), 
    
    # What was the primary motivation for purchasing insurance from a digital platform? 
    # For all respondents who selected Option 6 in I1
    
    fin_insur_dig_mot_exp = ifelse(I16 == 1, 1, 0), 
    fin_insur_dig_mot_cnv = ifelse(I16 == 2, 1, 0), 
    fin_insur_dig_mot_tru = ifelse(I16 == 3, 1, 0), 
    fin_insur_dig_mot_pro = ifelse(I16 == 4, 1, 0), 
    fin_insur_dig_mot_dat = ifelse(I16 == 5, 1, 0), 
    fin_insur_dig_mot_oth = ifelse(I16 == 6, 1, 0), 
    
    # Who would you go to if you faced any challenges with your insurance policy purchased through a digital platform? 
    # For all respondents who selected Option 6 in I1
    
    fin_insur_chlng_plt = ifelse(I17 == 1, 1, 0), 
    fin_insur_chlng_ins = ifelse(I17 == 2, 1, 0), 
    fin_insur_chlng_bth = ifelse(I17 == 3, 1, 0), 
    fin_insur_chlng_dk = ifelse(I17 %in% c(4,5), 1, 0), 
    
    # In the past three years (36 months), have you faced any issues in accessing and using your insurance policy?
    # For respondents who selected 'currently have' or 'had insurance, but not in the last 12 months' for any insurance in B1 in use of financial services section
    
    fin_insur_issues_type_any = ifelse(I18 == 1, 1, 0),
    
    #  What issues have you faced?
    # If the respondent selected ‘yes’ in question I18
    fin_insur_issues_type_lkc = ifelse(I19_1 == 1, 1, 0),
    fin_insur_issues_type_exp = ifelse(I19_2 == 1 | I19_8 == 1, 1, 0),
    fin_insur_issues_type_pre = ifelse(I19_3 == 1, 1, 0),
    fin_insur_issues_type_frd = ifelse(I19_4 == 1, 1, 0),
    fin_insur_issues_type_clm = ifelse(I19_5 == 1, 1, 0),
    fin_insur_issues_type_dat = ifelse(I19_6 == 1 | I19_7 == 1, 1, 0),
    fin_insur_issues_type_oth = ifelse(I19_9== 1, 1, 0),
    
    # What actions have you taken to address these issues? 
    # If the respondent selected ‘yes’ to any 1-9 in question I19
    fin_insur_issues_act_web = ifelse(I20_1 == 1, 1, 0), 
    fin_insur_issues_act_call = ifelse(I20_2 == 1 | I20_3 == 1, 1, 0), 
    fin_insur_issues_act_cht = ifelse(I20_4 == 1, 1, 0), 
    fin_insur_issues_act_ip = ifelse(I20_5 == 1 | I20_6 == 1, 1, 0), 
    fin_insur_issues_act_cg = ifelse(I20_7 == 1, 1, 0), 
    fin_insur_issues_act_dk = ifelse(I20_8 == 1, 1, 0), 
    fin_insur_issues_act_nr = ifelse(I20_9 == 1, 1, 0), 
    fin_insur_issues_act_oth = ifelse(I20_10 == 1, 1, 0), 
    
    #  To what extent do you trust insurance companies?
    # Ask all ,
    fin_insur_trust_yes = ifelse(I21 %in% c(1, 2), 1, 0), 
    fin_insur_trust_neu  = ifelse(I21 %in% c(3), 1, 0),
    fin_insur_trust_no  = ifelse(I21 %in% c(4, 5), 1, 0), 
      
    # Why do you not trust insurance companies? 
    fin_insur_trust_no_needs = ifelse(I22 == 1, 1, 0), 
    fin_insur_trust_no_fraud = ifelse(I22 %in% c(2, 4), 1, 0), # Included misuese of data
    fin_insur_trust_no_claim = ifelse(I22 == 3, 1, 0),
    fin_insur_trust_no_ux = ifelse(I22 %in% c(5, 6), 1, 0),
    fin_insur_trust_no_oth = ifelse(I22 == 7, 1, 0),
    
      ) %>% dummy_cols(select_columns = c("fin_insur_lif_shc", 
                                          "fin_insur_hlt_shc", 
                                          "fin_insur_acc_shc", 
                                          "fin_insur_fun_shc", 
                                          "fin_insur_hom_shc", 
                                          "fin_insur_bus_shc", 
                                          "fin_insur_aut_shc", 
                                          "fin_insur_idx_shc", 
                                          "fin_insur_oth_shc", 
                                          "risk_cli_inspur_shc", 
                                          "risk_hlt_insreco_shc", 
                                          "risk_hlt_inspur_shc", 
                                          "risk_hlt_claimdiff_shc", 
                                          "risk_hlt_claimquick_shc", 
                                          "risk_hlt_claimuse_shc")) -> raw_data
      
      raw_data %>% 
        mutate(
          fin_insur_lifhlt_slf = ifelse(fin_insur_lif_shc_cu == 1 | fin_insur_hlt_shc_cu == 1, fin_insur_lifhlt_slf, NA), 
          fin_insur_lifhlt_spo = ifelse(fin_insur_lif_shc_cu == 1 | fin_insur_hlt_shc_cu == 1, fin_insur_lifhlt_spo, NA), 
          fin_insur_lifhlt_chi = ifelse(fin_insur_lif_shc_cu == 1 | fin_insur_hlt_shc_cu == 1, fin_insur_lifhlt_chi, NA), 
          fin_insur_lifhlt_par = ifelse(fin_insur_lif_shc_cu == 1 | fin_insur_hlt_shc_cu == 1, fin_insur_lifhlt_par, NA), 
          fin_insur_lifhlt_oth = ifelse(fin_insur_lif_shc_cu == 1 | fin_insur_hlt_shc_cu == 1, fin_insur_lifhlt_par, NA), 
          
          fin_insur_hlt_op = ifelse(fin_insur_hlt_shc_cu == 1 | fin_insur_lif_shc_su == 1, fin_insur_hlt_op, NA), 
          fin_insur_hlt_ip = ifelse(fin_insur_hlt_shc_cu == 1 | fin_insur_lif_shc_su == 1, fin_insur_hlt_ip, NA), 
          fin_insur_hlt_sr = ifelse(fin_insur_hlt_shc_cu == 1 | fin_insur_lif_shc_su == 1, fin_insur_hlt_sr, NA), 
          fin_insur_hlt_vi = ifelse(fin_insur_hlt_shc_cu == 1 | fin_insur_lif_shc_su == 1, fin_insur_hlt_vi, NA), 
          fin_insur_hlt_dn = ifelse(fin_insur_hlt_shc_cu == 1 | fin_insur_lif_shc_su == 1, fin_insur_hlt_dn, NA), 
          fin_insur_hlt_ca = ifelse(fin_insur_hlt_shc_cu == 1 | fin_insur_lif_shc_su == 1, fin_insur_hlt_ca, NA), 
          fin_insur_hlt_di = ifelse(fin_insur_hlt_shc_cu == 1 | fin_insur_lif_shc_su == 1, fin_insur_hlt_di, NA), 
          fin_insur_hlt_em = ifelse(fin_insur_hlt_shc_cu == 1 | fin_insur_lif_shc_su == 1, fin_insur_hlt_em, NA), 
          fin_insur_hlt_px = ifelse(fin_insur_hlt_shc_cu == 1 | fin_insur_lif_shc_su == 1, fin_insur_hlt_px, NA), 
          fin_insur_hlt_tm = ifelse(fin_insur_hlt_shc_cu == 1 | fin_insur_lif_shc_su == 1, fin_insur_hlt_tm, NA), 
          fin_insur_hlt_ot = ifelse(fin_insur_hlt_shc_cu == 1 | fin_insur_lif_shc_su == 1, fin_insur_hlt_ot, NA)
          
        ) -> raw_data
    } 
    
      if (selected_country %in% c("Indonesia")) {
        
        raw_data %>% mutate(
          fin_insur_isl_shc = case_when(
            B1_10 == 1 ~ "cu", #Currently use
            B1_10 == 2 ~ "su", # Stopped using
            B1_10 == 3 ~ "nh", # Never had
            B1_10 == 4 ~ "na" # Not aware
          ), 
          fin_insur_isl_da = ifelse(B2_10 == 1, 1, ifelse(B2_10 == 2, 0, NA)), 
        ) %>% dummy_cols(select_columns = c("fin_insur_isl_shc")) -> raw_data 
        
      }
  
    if (selected_country %in% c("Indonesia", "Brazil")) { 
      
      if ("fin_insur_idx_shc_cu" %not_in% names(raw_data)) {
        raw_data %>% mutate(fin_insur_idx_shc_cu = 0) -> raw_data
      } 
      if ("fin_insur_lif_shc_cu" %not_in% names(raw_data)) {
        raw_data %>% mutate(fin_insur_lif_shc_cu = 0) -> raw_data
      }
      if ("fin_insur_hlt_shc_cu" %not_in% names(raw_data)) {
        raw_data %>% mutate(fin_insur_hlt_shc_cu = 0) -> raw_data
      }
      if ("fin_insur_acc_shc_cu" %not_in% names(raw_data)) {
        raw_data %>% mutate(fin_insur_acc_shc_cu = 0) -> raw_data
      }
      if ("fin_insur_fun_shc_cu" %not_in% names(raw_data)) {
        raw_data %>% mutate(fin_insur_fun_shc_cu = 0) -> raw_data
      }
      if ("fin_insur_fun_shc_su" %not_in% names(raw_data)) {
        raw_data %>% mutate(fin_insur_fun_shc_su = 0) -> raw_data
      }
      if ("fin_insur_hom_shc_cu" %not_in% names(raw_data)) {
        raw_data %>% mutate(fin_insur_hom_shc_cu = 0) -> raw_data
      } 
      if ("fin_insur_bus_shc_cu" %not_in% names(raw_data)) {
        raw_data %>% mutate(fin_insur_bus_shc_cu = 0) -> raw_data
      }
      if ("fin_insur_bus_shc_su" %not_in% names(raw_data)) {
        raw_data %>% mutate(fin_insur_bus_shc_su = 0) -> raw_data
      }
      if ("fin_insur_aut_shc_cu" %not_in% names(raw_data)) {
        raw_data %>% mutate(fin_insur_aut_shc_cu = 0) -> raw_data
      }
      if ("fin_insur_oth_shc_cu" %not_in% names(raw_data)) {
        raw_data %>% mutate(fin_insur_oth_shc_cu = 0) -> raw_data
      }
      if ("fin_insur_idx_shc_su" %not_in% names(raw_data)) {
        raw_data %>% mutate(fin_insur_idx_shc_su = 0) -> raw_data
      }
      if ("risk_hlt_claimuse_shc_dbt" %not_in% names(raw_data)) {
        raw_data %>% mutate(risk_hlt_claimuse_shc_dbt = 0) -> raw_data
      }
      if ("risk_hlt_insreco_shc_neu" %not_in% names(raw_data)) {
        raw_data %>% mutate(risk_hlt_insreco_shc_neu = 0) -> raw_data
      }
      
        raw_data %>% 
          mutate(
            fin_insur_any_shc_cu = ifelse(fin_insur_lif_shc_cu == 1 | fin_insur_hlt_shc_cu == 1 | fin_insur_acc_shc_cu == 1 | 
                                               fin_insur_fun_shc_cu == 1 | fin_insur_hom_shc_cu == 1 | fin_insur_bus_shc_cu == 1 | 
                                               fin_insur_aut_shc_cu == 1 | fin_insur_idx_shc_cu == 1 | fin_insur_oth_shc_cu == 1, 1, 0)
            
          ) -> raw_data
      } 
  
    if (selected_country %in% c("Ethiopia", "India", "Nigeria")) { 
      
      raw_data %>% mutate(
        # Currently uses any form of insurance
        fin_insur_lif_shc_cu = ifelse(Q44_1 == 1, 1, ifelse(Q44_1 == 2, 0, NA)), 
        fin_insur_hlt_shc_cu = ifelse(Q44_2 == 1, 1, ifelse(Q44_2 == 2, 0, NA)), 
        fin_insur_acc_shc_cu =  ifelse(Q44_3 == 1, 1, ifelse(Q44_3 == 2, 0, NA)), 
        fin_insur_bus_shc_cu =  ifelse(Q44_4 == 1, 1, ifelse(Q44_4 == 2, 0, NA)),   
        fin_insur_aut_shc_cu = ifelse(Q44_5 == 1, 1, ifelse(Q44_5 == 2, 0, NA)), 
        
        fin_insur_any_shc_cu = ifelse(fin_insur_lif_shc_cu == 1 | fin_insur_hlt_shc_cu == 1 | fin_insur_acc_shc_cu == 1 | fin_insur_bus_shc_cu == 1 | fin_insur_aut_shc_cu == 1, 1, 0), 
        
        # Access insurance digitally
        fin_insur_lif_da = ifelse(Q45_1 == 1, 1, ifelse(Q45_1 == 2, 0, NA)), 
        fin_insur_hlt_da = ifelse(Q45_2 == 1, 1, ifelse(Q45_2 == 2, 0, NA)), 
        fin_insur_acc_da = ifelse(Q45_3 == 1, 1, ifelse(Q45_3 == 2, 0, NA)), 
        fin_insur_bus_da = ifelse(Q45_4 == 1, 1, ifelse(Q45_4 == 2, 0, NA)), 
        fin_insur_aut_da = ifelse(Q45_5 == 1, 1, ifelse(Q45_5 == 2, 0, NA))
        
      ) -> raw_data
      
    }
      
    if ("fin_insur_hlt_shc_su" %not_in% names(raw_data)) {
      raw_data %>% mutate(fin_insur_hlt_shc_su = 0) -> raw_data
    }
  
    raw_data %>% 
      
    mutate(
      
      fin_insur_hlt_shc_cusu = ifelse(fin_insur_hlt_shc_cu == 1 | fin_insur_hlt_shc_su == 1, 1, 0), 
      fin_insur_hlt_status = ifelse(fin_insur_hlt_shc_cu == 1, "Has health insurance", "Does not have health insurance"), 
      fin_insur_any_shc_cu_comp = ifelse(fin_insur_lif_shc_cu == 1 | fin_insur_hlt_shc_cu == 1 | fin_insur_acc_shc_cu == 1 | fin_insur_bus_shc_cu == 1 | fin_insur_aut_shc_cu == 1, 1, 0), 
      
      # Mututally exclusive insurance adoption gorups (focuse on health and life)
      
      fin_ins_seg0_cu_hltlif = ifelse(fin_insur_any_shc_cu == 1 & (fin_insur_lif_shc_cu == 1 | fin_insur_hlt_shc_cu == 1), 1, 0), 
      fin_ins_seg0_cu_oth = ifelse(fin_insur_any_shc_cu == 1 & (fin_insur_lif_shc_cu == 0 & fin_insur_hlt_shc_cu == 0), 1, 0), 
      fin_ins_seg0_cu_none = ifelse(fin_insur_any_shc_cu == 0, 1, 0), 
      
      fin_ins_seg0_str = case_when(
        fin_ins_seg0_cu_hltlif == 1 ~ "Health or life insurance policyholder", 
        fin_ins_seg0_cu_oth == 1 ~ "Other insurance policyholder", 
        fin_ins_seg0_cu_none == 1 ~ "No insurance coverage", 
      ), 
      
      # Insurance segments: 
      
      fin_ins_seg1_cu = ifelse(fin_insur_any_shc_cu == 1, 1, 0), # Current user
      fin_ins_seg1_pr = ifelse(fin_insur_any_shc_cu == 0 & fin_insur_statm_5_num %in% c(4,5), 1, 0), # Prospective user: Likely to purchase (Strongly agree, Agree)
      fin_ins_seg1_hr = ifelse(fin_ins_seg1_cu == 0 & fin_ins_seg1_pr == 0, 1, 0), # Hard to reach
      
      fin_ins_seg2_cu = ifelse(fin_insur_any_shc_cu == 1, 1, 0), # Current user
      fin_ins_seg2_pr = ifelse(fin_insur_any_shc_cu == 0 & fin_insur_statm_5_num %in% c(3, 4,5), 1, 0), # Prospective user (Strongly agree, Agree, Neutral)
      fin_ins_seg2_hr = ifelse(fin_ins_seg2_cu == 0 & fin_ins_seg2_pr == 0, 1, 0), # Hard to reach
      
      fin_ins_seg3_cu = ifelse(fin_insur_any_shc_cu == 1, 1, 0), # Current user
      fin_ins_seg3_pr = ifelse(fin_insur_any_shc_cu == 0 & (fin_insur_statm_5_num %in% c(4,5) | fin_insur_statm_6_num %in% c(4,5) | fin_insur_statm_7_num %in% c(4,5)), 1, 0), # Prospective user: Likely to purchase (Strongly agree, Agree)
      fin_ins_seg3_hr = ifelse(fin_ins_seg3_cu == 0 & fin_ins_seg3_pr == 0, 1, 0), # Hard to reach
      
      fin_ins_seg3_str = case_when(
        fin_ins_seg3_cu == 1 ~ "Current policyholder", 
        fin_ins_seg3_pr == 1 ~ "Prospective policyholder", 
        fin_ins_seg3_hr == 1 ~ "Hard to reach", 
      ), 
      
      
      # User of digital financial services
      fin_dfs_user = ifelse(
        Q33 == 6 | 
          Q34 %in% c(2,3) | 
          Q35_5 == 1  | 
          Q35_6 == 1 | 
          Q35_7 == 1 | 
          Q35_8 == 1 | 
          Q37_6 == 1 | 
          Q37_7 == 1 | 
          Q38_1 == 1 |
          Q38_2 == 1 |
          Q38_3 == 1 |
          Q38_4 == 1 |
          Q38_5 == 1 |
          Q45_1 == 1 |
          Q45_2 == 1 |
          Q45_3 == 1 |
          Q45_4 == 1 |
          Q45_5 == 1 |
          B2_1 == 1 |
          B2_2 == 1 |
          B2_3 == 1 |
          B2_4 == 1 |
          B2_5 == 1 |
          B2_6 == 1 |
          B2_7 == 1 |
          B2_8 == 1 |
          B2_9 == 1 |
          B2_10 == 1, 1, 0), 
      
      fin_access_score = fin_account_formal + fin_merchpay_noncash + fin_activeloan_agg_any + fin_insur_any_shc_cu, 
      
      # Index of how closely tied up (only defined for respondents that are owners) business is with owner's personal finances
      business_hh_noboundaries = ifelse(business_premise_shc_1 == 1 & resp_pcthhincfrmbus_high == 1 & business_account_separate_v2 == 0, 1, 0),
      business_hh_noboundaries = ifelse(resp_type_owner == 1, business_hh_noboundaries, NA),
      business_hh_separate = ifelse(business_premise_shc_1 == 0 & business_account_separate_v2 == 1, 1, 0),
      business_hh_separate = ifelse(resp_type_owner == 1, business_hh_separate, NA),
      business_hh_codep_index = business_premise_shc_1 + resp_pcthhincfrmbus_high + business_finances_notseparate,
      business_hh_codep_index = ifelse(resp_type_owner == 1, business_hh_codep_index, NA),
      business_hh_codep_index = business_hh_codep_index/3 # Re-scaling so that index has range of 0-1

    ) -> raw_data
    
    # Financial access summary measure count of individual services (26 is max score)
    
    raw_data %>% 
      rowwise() %>% 
      mutate(
        fin_access_savings_N = sum(c(fin_bus_savings_cbnk, fin_bus_savings_mfi, fin_bus_savings_fintech, fin_bus_savings_sacco, fin_bus_savings_mm), na.rm = TRUE), 
        fin_access_loans_N = sum(c(fin_activeloan_cbnk, fin_activeloan_mfi, fin_activeloan_fintech, fin_activeloan_sacco, fin_activeloan_mm, fin_activeloan_platform, fin_owner_creditcard), na.rm = TRUE), 
        fin_access_payments_N = sum(c(fin_merchpay_poscard, fin_merchpay_bnktrnsf, fin_merchpay_online, fin_merchpay_mobmoney, fin_merchpay_instant, fin_merchpay_qr), na.rm = TRUE), 
        fin_access_insurance_N = sum(c(fin_insur_lif_shc_cu, fin_insur_hlt_shc_cu, fin_insur_acc_shc_cu, fin_insur_bus_shc_cu, fin_insur_aut_shc_cu), na.rm = TRUE), 
        fin_access_all_N = sum(c(fin_access_savings_N, fin_access_loans_N, fin_access_payments_N, fin_access_insurance_N), na.rm = TRUE)
      ) %>% ungroup() -> raw_data

    raw_data %>% 
      select(starts_with(c("country", "ID", "Initial_block_ID", "Cluster_number", "fullsample", "weight_msme", "adj", "p", "w", "business_", "resp_", "tech_", "fin_", "cp_", "perf_", "risk_", "resi_", "fx")))

}

runpca_psych <- function(data) { 
  
  prcomp(~ resp_motivation_entrep + resp_motivation_lackopp + resp_maingoal_grow + resp_maingoal_stab + resp_riskapproach_aggr + resp_riskapproach_avoid, scale = TRUE, data = data)

  }

runmca_psych <- function(data, showgraph = FALSE) { 
  
  data %>% 
    select(resp_motivation, resp_maingoal, resp_riskapproach) %>% filter(complete.cases(.)) %>% 
    mutate(
      resp_motivation = ifelse(resp_motivation == "Lack of other income opportunities or desire to generate additional income", "Lack other income opportunities", resp_motivation), 
      resp_maingoal = ifelse(resp_maingoal == "Leave business to get a regular wage paying job or other", "Leave business to get job", resp_maingoal), 
      resp_riskapproach = ifelse(resp_riskapproach == "Aggresively pursue risky opportunities that could generate significant additional income", "Aggressively pursue risky opportunities", resp_riskapproach), 
      resp_riskapproach = ifelse(resp_riskapproach == "Cautiously pursue risk opportunities that could generate a little income", "Cautiously pursue risky opportunities", resp_riskapproach), 
      resp_riskapproach = ifelse(resp_riskapproach == "Avoid risks even if they could generate additional income", "Avoid risks", resp_riskapproach), 
    ) %>%
    mutate(resp_motivation = str_wrap(resp_motivation, 20), 
           resp_maingoal = str_wrap(resp_maingoal, 20), 
           resp_riskapproach = str_wrap(resp_riskapproach, 20)) %>% 
    rename(Motivation = resp_motivation, `Main goal` = resp_maingoal, `Risk approach` = resp_riskapproach) -> data
  
  MCA(data, graph = showgraph)
  
}

add_pca_todata <- function(data) { 
  
  subset <- data %>% select(ID, resp_motivation, resp_maingoal, resp_riskapproach) %>% filter(complete.cases(.))
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
  ) %>% 
    separate_wider_delim(indicator_name, delim = ": ", names = c("indicator_group", "indicator_name"), too_few = "align_start") %>% 
    mutate(indicator_name_long = paste(indicator_group, indicator_name, sep = ": "))
    
  if (!is.null(keep)) {
    return(results %>% select(indicator, indicator_name_long, indicator_group, indicator_name, group, group_name, group_cat_val, nobs, starts_with(keep)))
  } else {
    return(results)
  }

}

compute_summary_clusterlevel_2g <- function(inidcators, groups_l1, groups_l2, data, psu = NULL, keep = NULL) {

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
  )  %>% 
    separate_wider_delim(indicator_name, delim = ": ", names = c("indicator_group", "indicator_name"), too_few = "align_start") %>% 
    mutate(indicator_name_long = paste(indicator_group, indicator_name, sep = ": "))

  if (!is.null(keep)) {
    return(results %>% select(!!sym(groups_l1), indicator, indicator_name_long, indicator_group, indicator_name, group, group_name, group_cat_val, nobs, starts_with(keep)))
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
  ) %>% 
    separate_wider_delim(indicator_name, delim = ": ", names = c("indicator_group", "indicator_name"), too_few = "align_start") %>% 
    mutate(indicator_name_long = paste(indicator_group, indicator_name, sep = ": "))

  if (!is.null(keep)) {
    return(results %>% select(indicator, indicator_name_long, indicator_group, indicator_name, group, group_name, group_cat_val, nobs, starts_with(keep)))
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
  ) %>% 
    separate_wider_delim(indicator_name, delim = ": ", names = c("indicator_group", "indicator_name"), too_few = "align_start") %>% 
    mutate(indicator_name_long = paste(indicator_group, indicator_name, sep = ": "))

  if (!is.null(keep)) {
    return(results %>% select(!!sym(groups_l1), indicator, indicator_name_long, indicator_group, indicator_name, group, group_name, group_cat_val, nobs, starts_with(keep)))
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

prep_fig <- function(terms, depvar_labels, effect_labels, depvarlog = FALSE, model_type_override = NULL) {
  
  terms %>% mutate(
    depvar_label = factor(depvar_labels[depvar], levels = depvar_labels, ordered = TRUE),
    effect_label = factor(effect_labels[term], levels = effect_labels, ordered = TRUE),
    fig_data = max(ifelse(term == "(Intercept)", estimate, NA), na.rm = TRUE),
    fig_data = ifelse(term != "(Intercept)", estimate + fig_data, estimate),
    model_type = factor(ifelse(confounds_flag == 0, "Model: Unadjusted", "Model: Adjusted"), levels = c("Model: Unadjusted", "Model: Adjusted"), ordered = TRUE)
  ) -> terms
  
  if(depvarlog) { 
    terms %>% mutate(fig_data = exp(fig_data)) -> terms
  }
  
  if (!is.null(model_type_override)){ 
    terms %>% mutate(
      model_type = model_type_override
    ) -> terms
  }
  
    return(terms)

}

model_and_prepfig <-function(depvar, maineffect, confounds, data, depvar_labels, effect_labels, selected_country = NULL, depvarlog = FALSE, model_type_override = NULL) {
  if (!is.null(selected_country)) { 
    data <- data %>% filter(country == selected_country)
    }
  terms <- capture_terms_clse(depvar, maineffect, confounds, data)
  fig <- prep_fig(terms, depvar_labels, effect_labels, depvarlog, model_type_override) %>% mutate(country = selected_country)
  return(fig)
}






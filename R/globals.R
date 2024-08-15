########################################################################

# Global parameters  --------------------------

#####################################################################################

COUNTRIES <- c("Indonesia", "Brazil", "Ethiopia", "Nigeria", "India")
CITIES <- c("Jakarta", "Sao Paulo", "Addis Ababa", "Lagos", "Delhi")
names(CITIES) <- COUNTRIES

WEIGHT_PARAMS <- list(
  "Indonesia" = list("N" = 26293, "n" = 96, "restricted_blocks" = c(23582, 26105, 21436, 16723)),
  "Brazil" = list("N" = NULL, "n" = NULL,  "restricted_blocks" = NULL),
  "Ethiopia" = list("N" = 15842, "n" = 100, "restricted_blocks" = NULL),
  "Nigeria" = list("N" = 9282, "n" = 100,  "restricted_blocks" = NULL),
  "India" = list("N" = 30880, "n" = 98,  "restricted_blocks" = NULL)
  )

# Exchange rates per USD at 01 June 2024 (LCU per USD) from UN Treasures
# https://treasury.un.org/operationalrates/OperationalRates.php

FX_RATES <- list(
  "Indonesia" = 16262,
  "Brazil" = 5.203,
  "Ethiopia" = 57.042,
  "Nigeria" = 1435.3,
  "India" = 83.31
)

INDICATORS <- c("N_business_total_percluster" = "Total businesses (N)",
                "business_total" = "Total businesses (N)",
                "resp_sex_men" = "Men",
                "resp_sex_women" = "Women",
                "resp_education_agg4_shc_non" = "Some primary or none",
                "resp_education_agg4_shc_pri" = "Completed primary or some secondary",
                "resp_education_agg4_shc_sec" = "Completed secondary or some university/technical school",
                "resp_education_agg4_shc_trt" = "Completed university/technical school",
                "resp_experience_agg5_shc_1" = "1 yr or less",
                "resp_experience_agg5_shc_2_5" = "2-5 yrs",
                "resp_experience_agg5_shc_6_10" = "6-10 yrs",
                "resp_experience_agg5_shc_11_20" = "11-20 yrs",
                "resp_experience_agg5_shc_20" = "> 20 yrs",
                "resp_age_agg6_shc_25" = "[< 25 yrs]",
                "resp_age_agg6_shc_2535" = "[25-35 yrs)",
                "resp_age_agg6_shc_3545" = "[35-45 yrs)",
                "resp_age_agg6_shc_4555" = "[45-55 yrs)",
                "resp_age_agg6_shc_5565" = "[55-65 yrs)",
                "resp_age_agg6_shc_65" = "[65+ yrs]",
                "resp_motivation_oth" = "Inherited business or other reason",
                "resp_motivation_lackopp" = "Lack of other income opportunities or desire to generate additional income",
                "resp_motivation_entrep" = "Desire to have own business",
                "resp_maingoal_grow"  = "Grow the business",
                "resp_maingoal_stab"  = "Cover business costs",
                "resp_maingoal_leave" = "Leave business to get a regular wage paying job or other",
                "resp_riskapproach_aggr" = "Aggresively pursue risky opportunities that could generate significant additional income",
                "resp_riskapproach_calc" = "Cautiously pursue risk opportunities that could generate a little income",
                "resp_riskapproach_avoid" = "Avoid risks even if they could generate additional income",
                "resp_psych_segment_shc_grow" = "Growth",
                "resp_psych_segment_shc_stab" = "Stability",
                "resp_psych_segment_shc_surv" = "Survival",
                "business_size_agg2_shc_1" = "Size: 1 person",
                "business_size_agg2_shc_2_10" = "Size: 2-10 people",
                "business_sector_agg2_shc_mfc" = "Sector: Manufacturing",
                "business_sector_agg2_shc_srv" = "Sector: Services",
                "business_premise_shc_1" = "Premises: Household",
                "business_premise_shc_2" = "Premises: Permanent" ,
                "business_premise_shc_3" = "Premises: Semi-permanent",
                "business_premise_shc_4" = "Premises: Other",
                "business_registered" = "Business is registered with tax collection bureau",
                "resp_pcthhincfrmbus_high" = "Business contributes over 50% of owner's household income",
                "business_finances_notseparate" = "Business either does not have a formal financial account or is not separate from owner's account",
                "business_hh_noboundaries" = "Business operations and finances are not separate from household",
                "business_hh_codep_index" = "Business-household co-dependence index",
                "tech_has_internet" = "Business has internet connectivity",
                "tech_has_device" = "Business uses mobile phone, tablet or computer",
                "tech_has_both" = "Business has internet and uses device",
                "tech_has_none" = "Business does not have internet and does not use device",
                "tech_cat_none" = "No digital technology used",
                "tech_cat_any1" = "Any 1 business function",
                "tech_cat_any2" = "Any 2 business functions",
                "tech_cat_any3" = "Any 3 business functions",
                "tech_cat_all4" = "All 4 business functions",
                "tech_uses_messaging" = "Uses messaging apps",
                "tech_uses_socialmedia" = "Uses social media",
                "tech_uses_website" = "Business has a website",
                "tech_uses_ecommerce" = "Uses e-commerce platforms",
                "tech_uses_software" = "Uses apps or software for business operations",
                "tech_uses_ai" = "Uses artificial intelligence",
                "tech_uses_digpayments" = "Uses digital payments",
                "tech_uses_digloans" = "Uses digital loans",
                "tech_uses_messaging_30da" = "Uses messaging apps (30-day active)",
                "tech_uses_socialmedia_30da" = "Uses social media (30-day active)",
                "tech_uses_ecommerce_30da" = "Uses e-commerce platforms (30-day active)",
                "tech_uses_website_imp" = "Website is very or somewhat important to business",
                "tech_uses_software_30da" = "Uses apps or software for business operations (30-day active)",
                "tech_function_comms" = "Communicating with customers",
                "tech_function_mkts" = "Accessing markets",
                "tech_function_ops" = "Enterprise operations",
                "tech_function_efin" = "Enterprise finance",
                "tech_function_comms_30da" = "Communicating with customers (30-day active)",
                "tech_function_mkts_30da" = "Accessing markets (30-day active)",
                "tech_function_ops_30da" = "Enterprise operations (30-day active)",
                "tech_function_efin_30da" = "Enterprise finance (30-day active)",
                "perf_rev" = "Revenues (past month) [LCU]",
                "perf_rev_usd" = "Revenues (past month) [USD]",
                "perf_revphrpemp" = "Hourly sales per employee [LCU]",
                "perf_revphrpemp_usd" = "Hourly sales per employee [USD]"
                )


GROUPS <- c("fullsample" = "All businesses",
            "business_premise" = "Premises type",
            "business_size_agg2" = "Business size",
            "business_sector_agg3" = "Industrial sector",
            "resp_experience_agg5" = "Experience running business (years)",
            "resp_sex_str" = "Gender of business owner or manager",
            "business_registration_status" = "Business registered with tax collection bureau?",
            "resp_psych_segment" = "Growth orienation segment",
            "resp_psych_segment_agg2" = "Growth orientation segment")

GROUP_CAT_LEVELS <- c("All businesses",
                      "Household premises", "Non-household premises with permanent structure", "Non-household premises with semi-permanent structure, including stalls or stands",
                      "Services: trade (re-sale)", "Services: other (eg. transport, construction)",
                      "1 person", "2-10 people",
                      "1 yr or less", "2-5 yrs", "6-10 yrs", "11-20 yrs", "> 20 yrs")



###---------------------------------------------------------------------###
###
### Get data from a humanitarian Multi-Sector Needs Assessment (MSNA)
### run in Haiti in 2022 by IMPACT Initiatives
###
###---------------------------------------------------------------------###
library(rio)
# https://www.impact-repository.org/document/reach/e2e66cb1/REACH_HTI_dataset_MSNA-2022.xlsx
hti_msna_2022 <- rio::import_list(
  "https://www.impact-repository.org/document/reach/e2e66cb1/REACH_HTI_dataset_MSNA-2022.xlsx",
  guess_max = 1048576
)
hti_msna_2022 <- readRDS("data-raw/REACH_HTI_dataset_MSNA-2022.RDS")

# Loal libraries
library(data.table)
library(SurveyLiteR)
main <- hti_msna_2022$main
setDT(main)

vars <- c(
  #------ Design

  # Unique id survey
  "uuid",
  # Stratum
  "stratum",
  # 1st stratum variable
  "admin1",
  # 2nd stratum variable
  "milieu",
  # Cluster id
  "i_cluster",
  # Weights
  "weights",

  #------ Disagg vars

  "c_chef_menage_genre", # gender of the head of household

  #------ Per type of analysis

  # For ratio
  "c_total_age_3a_17a", # number of children aged 3 to 17 years in the household
  "e_freq_regt", # formal school regular attendance
  "e_abandont", # school dropout

  # For mean
  "f_5_depenses_ba",
  "f_5_depenses_loyer",
  "f_5_depenses_eau",
  "f_5_depenses_bna_freq",
  "f_5_depenses_charges",
  "f_5_depenses_carburant",
  "f_5_depenses_transport",
  "f_5_depenses_communication",
  "f_5_depenses_intrants",

  # For median
  "f_5_depenses_ba",
  "f_5_depenses_loyer",
  "f_5_depenses_eau",
  "f_5_depenses_bna_freq",
  "f_5_depenses_charges",
  "f_5_depenses_carburant",
  "f_5_depenses_transport",
  "f_5_depenses_communication",
  "f_5_depenses_intrants",
  "f_1_consom_cereale",
  "f_1_consom_noix",
  "f_1_consom_lait",
  "f_1_consom_viande",
  "f_1_consom_legume",
  "f_1_consom_fruit",
  "f_1_consom_huile",
  "f_1_consom_sucre",

  # For select one
  "h_2_type_latrine", # type of latrines
  "r_1_reception_assistance", # reception of assistance in the last 12 months
  "hhs_cat", # Household Hunger Scale category
  "fcs_cat", # Food Consumption Score category
  "rcsi_cat", # reduced coping strategy index category
  "lcs_cat", # livelihood coping strategy index category

  # For select multiple
  "h_1_acces_eau", # issues with accessing water
  "r_1_besoin_prioritaire" # priority needs

  # For interaction
  # let's use the two above
)

# vars from main starting with "h_1_acces_eau"
vars <- c(vars, grep("^h_1_acces_eau_", colnames(main), value = TRUE))
# vars from main starting with "r_1_besoin_prioritaire"
vars <- c(vars, grep("^r_1_besoin_prioritaire_", colnames(main), value = TRUE))


# Select vars
main <- main[, ..vars]

# To ensure appropriate types, for instance weights is considered "character"
main[, weights := as.double(weights)]

# Sheet survey contains the survey sheet
survey <- hti_msna_2022$survey
setDT(survey)

# Split type column
survey <- split_survey(survey, "type")

# Rename one language label column to label
setnames(survey, "label_francais", "label")

# Keep only the design variables for the sake of simplicity
# Maybe to be changed later for checks
survey <- survey[name %in% vars]

# Sheet choices contains, well, the choices sheet
choices <- hti_msna_2022$choices
setDT(choices)

# Rename one language label column to label
setnames(choices, "label_francais", "label")

# Subset
choices <- choices[list_name %in% survey$list_name]

# remove duplicated columns, keeping the first instance
main <- main[, !duplicated(colnames(main)), with = FALSE]


# Now that the dataset and the Kobo tool are loaded, we can prepare the survey design:
design <- srvyr::as_survey_design(
  main,
  strata = stratum,
  weights = weights,
  ids = i_cluster
)


# Save data
usethis::use_data(main, overwrite = TRUE)
usethis::use_data(survey, overwrite = TRUE)
usethis::use_data(choices, overwrite = TRUE)
usethis::use_data(design, overwrite = TRUE)


# Data analysis plan ------------------------------------------------------

analysis_dap <- data.table::data.table(
  sector = c(
    rep("General information", 2),
    rep("Food Security", 10),
    rep("Expenses", 10),
    rep("Education", 3),
    rep("WASH", 2),
    rep("Livelihoods", 2),
    rep("Assistance", 2)
  ),
  indicator = c(
    # General information
    "Mean number of children aged 3-17 in household",
    "% of households by gender of head of household",

    # Food Security
    "% of households by Food Consumption Score category",
    "% of households by Household Hunger Scale category",
    "Mean days of cereal consumption per week",
    "Mean days of nuts consumption per week",
    "Mean days of dairy consumption per week",
    "Mean days of meat consumption per week",
    "Mean days of vegetables consumption per week",
    "Mean days of fruits consumption per week",
    "Mean days of oil consumption per week",
    "Mean days of sugar consumption per week",

    # Expenses
    "Median food expenses",
    "Median rent expenses",
    "Median water expenses",
    "Median frequent non-food expenses",
    "Median utility charges",
    "Median fuel expenses",
    "Median transport expenses",
    "Median communication expenses",
    "Median agricultural inputs expenses",
    "Mean food expenses",

    # Education
    "School dropout rate among children aged 3-17",
    "School attendance rate among children aged 3-17",
    "Mean number of children aged 3-17 in household",

    # WASH
    "% of households by type of latrine",
    "% of households by water access issues",

    # Livelihoods
    "% of households by Livelihood Coping Strategy category",
    "% of households by Reduced Coping Strategy Index category",

    # Assistance
    "% of households by assistance reception status",
    "% of households by priority needs"
  ),
  var = c(
    # General information
    "c_total_age_3a_17a",
    "c_chef_menage_genre",

    # Food Security
    "fcs_cat",
    "hhs_cat",
    "f_1_consom_cereale",
    "f_1_consom_noix",
    "f_1_consom_lait",
    "f_1_consom_viande",
    "f_1_consom_legume",
    "f_1_consom_fruit",
    "f_1_consom_huile",
    "f_1_consom_sucre",

    # Expenses
    "f_5_depenses_ba",
    "f_5_depenses_loyer",
    "f_5_depenses_eau",
    "f_5_depenses_bna_freq",
    "f_5_depenses_charges",
    "f_5_depenses_carburant",
    "f_5_depenses_transport",
    "f_5_depenses_communication",
    "f_5_depenses_intrants",
    "f_5_depenses_ba",

    # Education
    "e_abandont,c_total_age_3a_17a",
    "e_freq_regt,c_total_age_3a_17a",
    "c_total_age_3a_17a",

    # WASH
    "h_2_type_latrine",
    "h_1_acces_eau",

    # Livelihoods
    "lcs_cat",
    "rcsi_cat",

    # Assistance
    "r_1_reception_assistance",
    "r_1_besoin_prioritaire"
  ),
  analysis = c(
    "mean",

    rep("select_one", 3),
    rep("mean", 8),
    rep("median", 9),
    "mean",
    "ratio",
    "ratio",
    "mean",
    "select_one",
    "select_multiple",
    rep("select_one", 2),
    "select_one",
    "select_multiple"
  ),
  na_rm = c(
    "yes",
    "no",
    rep("no", 2),
    rep("yes", 8),
    rep("yes", 10),
    "yes",
    "no",
    "yes",
    "no",
    "yes",
    rep("no", 2),
    "no",
    "yes"
  ),
  subset = c(
    "Households with children aged 3-17",
    NA,
    rep(NA, 10),
    rep(NA, 10),
    rep("Households with children aged 3-17", 3),
    rep(NA, 2),
    rep(NA, 2),
    rep(NA, 2)
  ),
  disagg_var = NA_character_,
  var_label_clean = c(
    # General information
    "Children aged 3-17 in household - Integer",
    "Gender of head of household - Select one",

    # Food Security
    "Food Consumption Score categories - Select one",
    "Household Hunger Scale categories - Select one",
    "Cereal consumption frequency - Integer (0 to 7)",
    "Nuts consumption frequency - Integer (0 to 7)",
    "Dairy consumption frequency - Integer (0 to 7)",
    "Meat consumption frequency - Integer (0 to 7)",
    "Vegetables consumption frequency - Integer (0 to 7)",
    "Fruits consumption frequency - Integer (0 to 7)",
    "Oil consumption frequency - Integer (0 to 7)",
    "Sugar consumption frequency - Integer (0 to 7)",

    # Expenses
    "Food expenses - Numeric (Haitian Gourde)",
    "Rent expenses - Numeric (Haitian Gourde)",
    "Water expenses - Numeric (Haitian Gourde)",
    "Frequent non-food expenses - Numeric (Haitian Gourde)",
    "Utility charges - Numeric (Haitian Gourde)",
    "Fuel expenses - Numeric (Haitian Gourde)",
    "Transport expenses - Numeric (Haitian Gourde)",
    "Communication expenses - Numeric (Haitian Gourde)",
    "Agricultural inputs expenses - Numeric (Haitian Gourde)",
    "Food expenses - Numeric (Haitian Gourde)",

    # Education
    "School dropout - Binary",
    "School attendance regularity - Binary",
    "Children aged 3-17 in household - Integer",

    # WASH
    "Types of latrine - Select one",
    "Water access issues - Select multiple",

    # Livelihoods
    "Livelihood Coping Strategy categories - Select one",
    "Reduced Coping Strategy Index categories - Select one",

    # Assistance
    "Assistance reception status - Select one",
    "Priority needs - Select multiple"
  )
)

usethis::use_data(analysis_dap, overwrite = TRUE)

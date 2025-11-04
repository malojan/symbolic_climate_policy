# =============================================================================
# 04-study-03-cleaning.R
# -----------------------------------------------------------------------------
# Purpose   : Clean and recode the dataset for replication of Study 3
# Authors   : Malo Jan & Luis Sattelmayer
# =============================================================================
# Description:
#   This script prepares the dataset for Study 3 by:
#     1. Importing the raw sav data
#     2. Recoding demographic and attitudinal variables
#     3. Recoding treatment and outcome variables
#     4. Handling missing values (mean imputation for continuous variables)
#     5. Selecting analysis-relevant variables
#     6. Exporting a cleaned dataset for downstream analysis
#
# Inputs:
#   - data/raw/study-03-data-raw.sav
#
# Outputs:
#   - data/processed/data-study-03-clean.rds
#
# Dependencies:
#   haven, tidyverse, here, glue
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------


library(haven)
library(tidyverse)

# -----------------------------------------------------------------------------
# 1. Import raw data
# -----------------------------------------------------------------------------

data <- read_sav("data/raw/study-03-data-raw.sav")

# -----------------------------------------------------------------------------
# 2. Recode demographic and attitudinal variables
# -----------------------------------------------------------------------------

data_clean <- data |> 
  mutate(
    # GENDER
    gender = case_when(
      Q1 == 1 ~ "Male", 
      Q1 == 2 ~ "Female"
    ) |> as.factor(),
    # URBAN vs RURAL
    urban_rural_cat = case_when(
      CC == 1 ~ "Rural",
      CC == 2 ~ "Inf 19999",
      CC == 3 ~ "20000-99999",
      CC == 4 ~ "Sup 100000",
      CC == 5 ~ "Agglo Paris",
      .default = 
    ) |> as.factor(),
    # EDUCATION
    education_cat = case_when(
      Q5 == 1 ~ "NO",
      Q5 == 2 ~ "CAP_BEPC",
      Q5 == 3 ~ "BAC",
      Q5 == 4 ~ "BAC+2",
      Q5 == 5 ~ "BAC+3_plus"
    ) |> as.factor(),
    # AGE
    age_cat_7 = case_when(
      rec_age == 1 ~ "18-24",
      rec_age == 2 ~ "25-34",
      rec_age == 3 ~ "35-44",
      rec_age == 4 ~ "45-54",
      rec_age == 5 ~ "55-64",
      rec_age == 6 ~ "65-74",
      rec_age == 7 ~ "75+"
    ) |> as.factor(),
    age_num = 2024 - Q2,
    # INCOME
    income = case_when(
      Q7 %in% c(1:8) ~ Q7,
      .default = NA_real_
    ),
    # Ideology
    ideology = case_when(
      Q10 %in% c(0:10) ~ Q10,
      .default = NA_real_
    ),
    # CLIMATE CONCERN
    climate_concern = case_when(
      Q8 %in% c(1:5) ~ Q8,
      .default = NA_real_
    ),
    # GOVERNMENT SATISFACTION
    gov_sati = case_when(
      Q9 %in% c(1:4) ~ Q9,
      .default = NA_real_
    ),
    # CAR USE
    car_main_transport = case_when(
      Q11 == 1 ~ "Yes", 
      Q11 == 2 ~ "No",
      .default = "No"
    ) |> as.factor()
  ) |> 
  rename(weight = Weight)

# -----------------------------------------------------------------------------
# 3. Recode treatments and outcome variables
# -----------------------------------------------------------------------------

data_clean <- data_clean |> 
  mutate(
    support_highway = case_when(
      Q12_E1G1 == 1 | Q12_1_E1G2 == 1 | Q12_1_E1G3 == 1 | Q12_1_E1G4 == 1 | Q12_1_E1G5 == 1 ~ 4,
      Q12_E1G1 == 2 | Q12_1_E1G2 == 2 | Q12_1_E1G3 == 2 | Q12_1_E1G4 == 2 | Q12_1_E1G5 == 2 ~ 3,
      Q12_E1G1 == 3 | Q12_1_E1G2 == 3 | Q12_1_E1G3 == 3 | Q12_1_E1G4 == 3 | Q12_1_E1G5 == 3 ~ 2,
      Q12_E1G1 == 4 | Q12_1_E1G2 == 4 | Q12_1_E1G3 == 4 | Q12_1_E1G4 == 4 | Q12_1_E1G5 == 4 ~ 1
    ),
    justice_highway = case_when(
      Q12_2_E1G1 == 4 | Q12_2_E1G2 == 4 | Q12_2_E1G3 == 4 | Q12_2_E1G4 == 4 |  Q12_2_E1G5 == 4 ~ 4,
      Q12_2_E1G1 == 3 | Q12_2_E1G2 == 3 | Q12_2_E1G3 == 3 | Q12_2_E1G4 == 3 |  Q12_2_E1G5 == 3 ~ 3,
      Q12_2_E1G1 == 2 | Q12_2_E1G2 == 2 | Q12_2_E1G3 == 2 | Q12_2_E1G4 == 2 |  Q12_2_E1G5 == 2 ~ 2,
      Q12_2_E1G1 == 1 | Q12_2_E1G2 == 1 | Q12_2_E1G3 == 1 | Q12_2_E1G4 == 1 |  Q12_2_E1G5 == 1 ~ 1
    ),
    effective_highway = case_when(
      Q12_3_E1G1 == 1 | Q12_3_E1G2 == 1 | Q12_3_E1G3 == 1 | Q12_3_E1G4 == 1 | Q12_3_E1G5 == 1 ~ 4,
      Q12_3_E1G1 == 2 | Q12_3_E1G2 == 2 | Q12_3_E1G3 == 2 | Q12_3_E1G4 == 2 | Q12_3_E1G5 == 2 ~ 3,
      Q12_3_E1G1 == 3 | Q12_3_E1G2 == 3 | Q12_3_E1G3 == 3 | Q12_3_E1G4 == 3 | Q12_3_E1G5 == 3 ~ 2,
      Q12_3_E1G1 == 4 | Q12_3_E1G2 == 4 | Q12_3_E1G3 == 4 | Q12_3_E1G4 == 4 | Q12_3_E1G5 == 4 ~ 1
    ),
    seriousness_highway = case_when(
      Q12_4_E1G1 == 1 | Q12_4_E1G2 == 1 | Q12_4_E1G3 == 1 | Q12_4_E1G4 == 1 | Q12_4_E1G5 == 1 ~ 1,
      Q12_4_E1G1 == 2 | Q12_4_E1G2 == 2 | Q12_4_E1G3 == 2 | Q12_4_E1G4 == 2 | Q12_4_E1G5 == 2 ~ 2,
      Q12_4_E1G1 == 3 | Q12_4_E1G2 == 3 | Q12_4_E1G3 == 3 | Q12_4_E1G4 == 3 | Q12_4_E1G5 == 3 ~ 3,
      Q12_4_E1G1 == 4 | Q12_4_E1G2 == 4 | Q12_4_E1G3 == 4 | Q12_4_E1G4 == 4 | Q12_4_E1G5 == 4 ~ 4
    ),
    elite_highway = case_when(
      Q12_5_E1G1 == 1 | Q12_5_E1G2 == 1 | Q12_5_E1G3 == 1 | Q12_5_E1G4 == 1 | Q12_5_E1G5 == 1 ~ 4,
      Q12_5_E1G1 == 2 | Q12_5_E1G2 == 2 | Q12_5_E1G3 == 2 | Q12_5_E1G4 == 2 | Q12_5_E1G5 == 2 ~ 3,
      Q12_5_E1G1 == 3 | Q12_5_E1G2 == 3 | Q12_5_E1G3 == 3 | Q12_5_E1G4 == 3 | Q12_5_E1G5 == 3 ~ 2,
      Q12_5_E1G1 == 4 | Q12_5_E1G2 == 4 | Q12_5_E1G3 == 4 | Q12_5_E1G4 == 4 | Q12_5_E1G5 == 4 ~ 1
    ),
    support_carbon_tax = case_when(
      Q13_1_E2G1 == 1 | Q13_1_E2G2 == 1 | Q13_1_E2G3 == 1 | Q13_1_E2G4 == 1 | Q13_1_E2G5 == 1 ~ 4,
      Q13_1_E2G1 == 2 | Q13_1_E2G2 == 2 | Q13_1_E2G3 == 2 | Q13_1_E2G4 == 2 | Q13_1_E2G5 == 2 ~ 3,
      Q13_1_E2G1 == 3 | Q13_1_E2G2 == 3 | Q13_1_E2G3 == 3 | Q13_1_E2G4 == 3 | Q13_1_E2G5 == 3 ~ 2,
      Q13_1_E2G1 == 4 | Q13_1_E2G2 == 4 | Q13_1_E2G3 == 4 | Q13_1_E2G4 == 4 | Q13_1_E2G5 == 4 ~ 1
    ),
    justice_carbon_tax = case_when(
      Q13_2_E2G1 == 1 | Q13_2_E2G2 == 1 | Q13_2_E2G3 == 1 | Q13_2_E2G4 == 1 | Q13_2_E2G5 == 1 ~ 4,
      Q13_2_E2G1 == 2 | Q13_2_E2G2 == 2 | Q13_2_E2G3 == 2 | Q13_2_E2G4 == 2 | Q13_2_E2G5 == 2 ~ 3,
      Q13_2_E2G1 == 3 | Q13_2_E2G2 == 3 | Q13_2_E2G3 == 3 | Q13_2_E2G4 == 3 | Q13_2_E2G5 == 3 ~ 2,
      Q13_2_E2G1 == 4 | Q13_2_E2G2 == 4 | Q13_2_E2G3 == 4 | Q13_2_E2G4 == 4 | Q13_2_E2G5 == 4 ~ 1
    ),
    effective_carbon_tax = case_when(
      Q13_3_E2G1 == 1 | Q13_3_E2G2 == 1 | Q13_3_E2G3 == 1 | Q13_3_E2G4 == 1 | Q13_3_E2G5 == 1 ~ 4,
      Q13_3_E2G1 == 2 | Q13_3_E2G2 == 2 | Q13_3_E2G3 == 2 | Q13_3_E2G4 == 2 | Q13_3_E2G5 == 2 ~ 3,
      Q13_3_E2G1 == 3 | Q13_3_E2G2 == 3 | Q13_3_E2G3 == 3 | Q13_3_E2G4 == 3 | Q13_3_E2G5 == 3 ~ 2,
      Q13_3_E2G1 == 4 | Q13_3_E2G2 == 4 | Q13_3_E2G3 == 4 | Q13_3_E2G4 == 4 | Q13_3_E2G5 == 4 ~ 1
    ),
    seriousness_carbon_tax = case_when(
      Q13_4_E2G1 == 1 | Q13_4_E2G2 == 1 | Q13_4_E2G3 == 1 | Q13_4_E2G4 == 1 | Q13_4_E2G5 == 1 ~ 1,
      Q13_4_E2G1 == 2 | Q13_4_E2G2 == 2 | Q13_4_E2G3 == 2 | Q13_4_E2G4 == 2 | Q13_4_E2G5 == 2 ~ 2,
      Q13_4_E2G1 == 3 | Q13_4_E2G2 == 3 | Q13_4_E2G3 == 3 | Q13_4_E2G4 == 3 | Q13_4_E2G5 == 3 ~ 3,
      Q13_4_E2G1 == 4 | Q13_4_E2G2 == 4 | Q13_4_E2G3 == 4 | Q13_4_E2G4 == 4 | Q13_4_E2G5 == 4 ~ 4
    ),
    elite_carbon_tax = case_when(
      Q13_5_E2G1 == 1 | Q13_5_E2G2 == 1 | Q13_5_E2G3 == 1 | Q13_5_E2G4 == 1 | Q13_5_E2G5 == 1 ~ 4,
      Q13_5_E2G1 == 2 | Q13_5_E2G2 == 2 | Q13_5_E2G3 == 2 | Q13_5_E2G4 == 2 | Q13_5_E2G5 == 2 ~ 3,
      Q13_5_E2G1 == 3 | Q13_5_E2G2 == 3 | Q13_5_E2G3 == 3 | Q13_5_E2G4 == 3 | Q13_5_E2G5 == 3 ~ 2,
      Q13_5_E2G1 == 4 | Q13_5_E2G2 == 4 | Q13_5_E2G3 == 4 | Q13_5_E2G4 == 4 | Q13_5_E2G5 == 4 ~ 1
    ),
    treatment_highway = case_when(
      sel_ex1 == 1 ~ "Control",
      sel_ex1 == 2 ~ "Pure Symbolic",
      sel_ex1 == 3 ~ "Costly + Symbolic",
      sel_ex1 == 4 ~ "Costly No Symbolic",
      sel_ex1 == 5 ~ "Symbolic No Costly"
    ) |> fct_relevel("Costly No Symbolic", "Control", "Costly + Symbolic", "Pure Symbolic","Symbolic No Costly"),
    treatment_carbon_tax = case_when(
      sel_ex2 == 1 ~ "Control",
      sel_ex2 == 2 ~ "Pure Symbolic",
      sel_ex2 == 3 ~ "Costly + Symbolic",
      sel_ex2 == 4 ~ "Costly No Symbolic",
      sel_ex2 == 5 ~ "Symbolic No Costly"
    ) |> fct_relevel("Costly No Symbolic", "Control", "Costly + Symbolic", "Pure Symbolic","Symbolic No Costly")
  )

# -----------------------------------------------------------------------------
# 4. Handle missing values (mean imputation for continuous variables)
# -----------------------------------------------------------------------------

data_clean <- data_clean |> 
  mutate(
    income = case_when(
      income %in% c(1:8) ~ income, 
      is.na(income) ~ mean(income, na.rm = TRUE)
    ),
    ideology = case_when(
      ideology %in% c(0:10) ~ ideology, 
      is.na(ideology) ~ mean(ideology, na.rm = TRUE)
    ),
    gov_sati = case_when(
      gov_sati %in% c(1:4) ~ gov_sati, 
      is.na(gov_sati) ~ mean(gov_sati, na.rm = TRUE)
    ),
    climate_concern = case_when(
      climate_concern %in% c(1:5) ~ climate_concern, 
      is.na(climate_concern) ~ mean(climate_concern, na.rm = TRUE)
    )
  )

# -----------------------------------------------------------------------------
# 6. Select analysis variables
# -----------------------------------------------------------------------------

data_clean <- data_clean |> 
  select(
    gender, age_num, urban_rural_cat, education_cat, income, ideology, gov_sati, climate_concern,
    car_main_transport, weight, starts_with("support"), starts_with("effective_"), starts_with("seriousness_"), starts_with("elite_"),
    starts_with("treatment_"), starts_with("justice")
  )

# -----------------------------------------------------------------------------
# 7. Export cleaned dataset
# -----------------------------------------------------------------------------

output_path <- here("data/processed/data-study-03-clean.rds")
write_rds(data_clean, output_path)

message(glue::glue("Cleaned Study 3 dataset saved to {output_path}"))


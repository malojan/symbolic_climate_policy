# =============================================================================
# 01-study-01-cleaning.R
# -----------------------------------------------------------------------------
# Purpose   : Clean and recode the BEE dataset for replication of Study 1
# Author : Malo Jan
# =============================================================================
# Description:
#   This script performs the following steps:
#     1. Imports the raw BEE dataset (download from data.sciencespo.fr)
#     2. Recodes demographic, attitudinal, and treatment variables
#     3. Handles missing values using mean imputation
#     4. Constructs a Climate Policy Support Index using PCA + HCPC clustering
#     5. Exports a cleaned dataset for downstream analyses
#
# Usage:
#   source("code/setup_environment.R")   # activates renv environment
#   source("code/01-study-01-cleaning.R")
#
# Input  : data/raw/fr_cdsp_ddi_elipss_202312_bee.csv
# Output : data/processed/study-01-clean-data.rds
#          data/processed/study-01-climate-policy-support-data.rds
# Figures: outputs/figures/appendix-figure-11.png 
#          outputs/figures/appendix-figure-12.png 
#          outputs/figures/appendix-figure-13.png
# =============================================================================

# 0. Setup---------------------------------------------------------------------

# Load required packages

library(tidyverse)
library(here)
library(FactoMineR)
library(janitor)
library(factoextra)

# --- 1. Import raw data -------------------------------------------------------
# The CSV file must be manually downloaded from data.sciencespo.fr
# DOI: https://doi.org/10.21410/7E4/OH0RKI
# and placed in data/raw/

raw_file <- here("data/raw/fr_cdsp_ddi_elipss_202312_bee.csv")

if (!file.exists(raw_file)) {
  stop(glue::glue(
    "Data file not found: {raw_file}\n",
    "Please download it from https://doi.org/10.21410/7E4/OH0RKI ",
    "and place it in the 'data/raw/' folder."
  ))
}

bee <- read_csv(raw_file)

# -----------------------------------------------------------------------------
# 2. Recode demographic and attitudinal variables
# -----------------------------------------------------------------------------

bee <- bee |>
  mutate(
    # Gender
    gender = case_when(
      eayy_a1 == 1 ~ "Male",
      eayy_a1 == 2 ~ "Female",
      TRUE ~ NA_character_
    ) |> as.factor(),
    
    # Urban vs. rural (agglomeration size)
    urban_rural_num = case_when(
      cal_TUU == 0 ~ 0,
      cal_TUU == 1 ~ 1,
      cal_TUU == 4 ~ 2,
      cal_TUU == 6 ~ 3,
      cal_TUU == 8 ~ 4,
      TRUE ~ NA_real_
    ),
    
    # Education
    education_cat = case_when(
      cal_DIPL == 1 ~ "DIPL_SUP",
      cal_DIPL == 2 ~ "BAC_BAC2",
      cal_DIPL == 3 ~ "CAP_BEPC",
      cal_DIPL == 4 ~ "NO",
      TRUE ~ NA_character_
    ),
    
    # Age (12 categories)
    age_num_12 = case_when(
      eayy_a2a_rec2 %in% 1:15 ~ eayy_a2a_rec2,
      TRUE ~ NA_real_
    ),
    
    # Income
    income = case_when(
      eayy_e2auc %in% 1:10 ~ eayy_e2auc,
      TRUE ~ NA_real_
    ),
    
    # Ideology (0–10 scale)
    ideology = case_when(
      eayy_i8 %in% 0:10 ~ eayy_i8,
      TRUE ~ NA_real_
    ),
    
    # Climate concern
    climate_concern = case_when(
      bee0_A13 %in% 1:5 ~ bee0_A13,
      TRUE ~ NA_real_
    ),
    
    # Government satisfaction (reverse-coded)
    gov_sati_num = case_when(
      bee0_B1 == 1 ~ 4,
      bee0_B1 == 2 ~ 3,
      bee0_B1 == 3 ~ 2,
      bee0_B1 == 4 ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Car as main transport
    car_main_transport = case_when(
      bee0_C31 == 1 ~ "Yes",
      TRUE ~ "No"
    ) |> as.factor(),
    
    # Flight use
    flight_use = case_when(
      bee0_C35 >= 1 ~ "Yes",
      TRUE ~ "No"
    ) |> as.factor(),
    POIDS_bee0 = str_replace(POIDS_bee0, ",", ".") |> as.numeric()
  )

# --- 3. Recode treatments and outcome variables -------------------------------

bee <- bee |>
  mutate(
    # --- Highway experiment ---
    support_highway = case_when(
      bee0_GRAC == 1 | bee0_GRA1 == 1 | bee0_GRA2 == 1 ~ "Totally agree",
      bee0_GRAC == 2 | bee0_GRA1 == 2 | bee0_GRA2 == 2 ~ "Rather agree",
      bee0_GRAC == 3 | bee0_GRA1 == 3 | bee0_GRA2 == 3 ~ "Rather disagree",
      bee0_GRAC == 4 | bee0_GRA1 == 4 | bee0_GRA2 == 4 ~ "Totally disagree",
      TRUE ~ NA_character_
    ) |> fct_relevel("Totally disagree", "Rather disagree", "Rather agree", "Totally agree"),
    
    support_highway_num = case_when(
      support_highway == "Totally agree" ~ 4,
      support_highway == "Rather agree" ~ 3,
      support_highway == "Rather disagree" ~ 2,
      support_highway == "Totally disagree" ~ 1,
      TRUE ~ NA_real_
    ),
    
    treatment_highway = case_when(
      bee0_GRAC != 6666 ~ "Control",
      bee0_GRA1 != 6666 ~ "Ministers",
      bee0_GRA2 != 6666 ~ "Rich",
      TRUE ~ NA_character_
    ) |> fct_relevel("Control", "Rich", "Ministers"),
    
    # --- Flights experiment ---
    support_flights = case_when(
      bee0_GRBC == 1 | bee0_GRB1 == 1 | bee0_GRB2 == 1 ~ "Totally agree",
      bee0_GRBC == 2 | bee0_GRB1 == 2 | bee0_GRB2 == 2 ~ "Rather agree",
      bee0_GRBC == 3 | bee0_GRB1 == 3 | bee0_GRB2 == 3 ~ "Rather disagree",
      bee0_GRBC == 4 | bee0_GRB1 == 4 | bee0_GRB2 == 4 ~ "Totally disagree",
      TRUE ~ NA_character_
    ) |> fct_relevel("Totally disagree", "Rather disagree", "Rather agree", "Totally agree"),
    
    support_flights_num = case_when(
      support_flights == "Totally agree" ~ 4,
      support_flights == "Rather agree" ~ 3,
      support_flights == "Rather disagree" ~ 2,
      support_flights == "Totally disagree" ~ 1,
      TRUE ~ NA_real_
    ),
    
    treatment_flights = case_when(
      bee0_GRBC != 6666 ~ "Control",
      bee0_GRB1 != 6666 ~ "Ministers",
      bee0_GRB2 != 6666 ~ "Rich",
      TRUE ~ NA_character_
    ) |> fct_relevel("Control", "Rich", "Ministers")
  )

# --- 4. Handle missing values -------------------------------------------------

bee <- bee |>
  mutate(across(
    c(income, ideology, gov_sati_num, climate_concern),
    ~ if_else(is.na(.x), mean(.x, na.rm = TRUE), .x)
  ))

# --- 5. Construct Climate Policy Support Index (PCA + clustering) ------------

# Extract climate policy support items

cp_data <- bee |> 
  select(bee0_B9:bee0_B19) |> 
  pivot_longer(cols = bee0_B9:bee0_B19, names_to = "issue_policy", values_to = "support") |> 
  mutate(issue_policy = case_when(
    issue_policy == "bee0_B9" ~ "car_city_center",
    issue_policy == "bee0_B10" ~ "tax_fossil",
    issue_policy == "bee0_B11" ~ "renov",
    issue_policy == "bee0_B12" ~ "nuc",
    issue_policy == "bee0_B13" ~ "tax_flights",
    issue_policy == "bee0_B14" ~ "wind",
    issue_policy == "bee0_B15" ~ "car_new",
    issue_policy == "bee0_B16" ~ "buildings",
    issue_policy == "bee0_B17" ~ "norm_agri",
    issue_policy == "bee0_B18" ~ "vege",
    issue_policy == "bee0_B19" ~ "transport_sub"
  ), 
  support = case_when(
    support == "9999" ~ NA_real_,
    .default = support
  )) |> 
  pivot_wider(names_from = "issue_policy", values_from = "support") |> 
  unnest() |> 
  select(- nuc) |> 
  # Rename all columns by adding cp_ before
  rename_with(~ paste0("cp_", .)) |> 
  bind_cols(bee) |> 
  select(starts_with("cp_"))

write_rds(cp_data, "data/processed/study-01-climate-policy-support-data.rds")

# Principal Component Analysis (PCA)
cp_pca <- FactoMineR::PCA(cp_data, scale.unit = TRUE, graph = FALSE)

fviz_pca_var(cp_pca,  repel = TRUE) 

ggsave("outputs/figures/appendix-figure-11.png")

# Hierarchical clustering on principal components
hcpc <- FactoMineR::HCPC(cp_pca, graph = FALSE)

factoextra::fviz_cluster(hcpc,
                         repel = FALSE,            # Avoid label overlapping
                         show.clust.cent = TRUE, # Show cluster centers
                         palette = "jco",         # Color palette see ?ggpubr::ggpar
                         ggtheme = theme_minimal(),
                         # Change color to grey
                         
                         
                         main = "Factor map",
                         # Remove labels
                         geom = "point",
)  

ggsave("outputs/figures/appendix-figure-12.png")

hcpc_clusters <- hcpc$data.clust |> as_tibble() |> 
  select(clust)

hcpc_clusters |>
  mutate(
    cluster = case_when(
      clust == 1 ~ "Pro-climate policy",
      clust == 2 ~ "Moderate climate-policy",
      clust == 3 ~ "Anti-climate policy"
    )
  ) |> 
  ggplot() +
  geom_bar(aes(x = factor(cluster), ..prop.., group = 1)) +
  scale_x_discrete('Cluster') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light()

ggsave("outputs/figures/appendix-figure-13.png")

# Extract PCA coordinates and cluster assignments
cp_coords <- cp_pca$ind$coord |>
  as_tibble() |>
  clean_names() |>
  select(dim_1, dim_2) |>
  bind_cols(cp_data) |>
  bind_cols(as_tibble(hcpc$data.clust["clust"])) |>
  mutate(
    cp_index  = dim_1,
    cp_index2 = dim_2,
    cp_cluster = case_when(
      clust == 1 ~ "Pro-climate policy",
      clust == 2 ~ "Moderate climate policy",
      clust == 3 ~ "Anti-climate policy",
      TRUE ~ NA_character_
    )
  )

# Add climate policy index and clusters to main dataset

bee <- bind_cols(bee, cp_coords)

# --- 6. Select final analysis variables --------------------------------------

bee <- bee |>
  select(
    support_highway_num, treatment_highway,
    support_flights_num, treatment_flights,
    ideology, gov_sati_num, gender, income, age_num_12,
    education_cat, urban_rural_num, car_main_transport, flight_use,
    climate_concern, cp_index, cp_cluster, POIDS_bee0
  )

# --- 7. Export cleaned dataset -----------------------------------------------

output_path <- here("data/processed/study-01-clean-data.rds")
write_rds(bee, output_path)

message(glue::glue("Cleaned Study 1 dataset saved to {output_path}"))


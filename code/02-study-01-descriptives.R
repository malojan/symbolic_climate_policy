# =============================================================================
# 02-study-01-descriptives.R
# Purpose  : Plot descriptive figures for appendix
# Author  : Malo Jan
# =============================================================================
# Description:
#   This script:
#     1. Imports the cleaned BEE dataset
#     2. Generates descriptive plots for key socio-demographic
#        and attitudinal variables
#     3. Exports all figures as PDF files for the replication appendix
#
# Usage:
#   source("code/00-setup-environmnet") 
#   source("code/02-study-01-descriptives.R")
#
# Inputs:
#   - data/processed/study-01-data-clean.rds
#
# Outputs:
#   - outputs/figures/appendix-figure-01.pdf : Ideology distribution
#   - outputs/figures/appendix-figure-02.pdf : Government satisfaction
#   - outputs/figures/appendix-figure-03.pdf : Gender distribution
#   - outputs/figures/appendix-figure-04.pdf : Education level
#   - outputs/figures/appendix-figure-05.pdf : Income distribution
#   - outputs/figures/appendix-figure-06.pdf : Urban/rural distribution
#   - outputs/figures/appendix-figure-07.pdf : Age distribution
#   - outputs/figures/appendix-figure-08.pdf : Car as main transport
#   - outputs/figures/appendix-figure-09.pdf : Flight use
#   - outputs/figures/appendix-figure-10.pdf : Climate concern
#
# Dependencies:
#   - R packages: tidyverse, here, scales
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------

library(tidyverse)
library(here)
library(scales)

# --- 1. Load cleaned data -----------------------------------------------------

bee <- read_rds(here("data/processed/study-01-data-clean.rds"))


# -----------------------------------------------------------------------------
# 2. Ideology distribution
# -----------------------------------------------------------------------------

bee |>
  mutate(
    ideology = case_when(
      ideology > 4 & ideology < 5 ~ NA_real_,
      .default = ideology
    )
  ) |>
  ggplot() +
  geom_bar(aes(factor(ideology), ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_y_continuous("Share", labels = percent) +
  theme_light() +
  scale_x_discrete("Left-right self-placement")

ggsave(here("outputs/figures/appendix-figure-01.pdf"))


# -----------------------------------------------------------------------------
# 3. Government satisfaction
# -----------------------------------------------------------------------------

bee |>
  mutate(
    gov_sati = case_when(
      gov_sati_num == 4 ~ "Very satisfied",
      gov_sati_num == 3 ~ "Rather satisfied",
      gov_sati_num == 2 ~ "Rather dissatisfied",
      gov_sati_num == 1 ~ "Very dissatisfied"
    ) |> fct_relevel(
      "Very dissatisfied", "Rather dissatisfied",
      "Rather satisfied", "Very satisfied"
    )
  ) |>
  ggplot() +
  geom_bar(aes(x = factor(gov_sati), ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete("Satisfaction toward government") +
  scale_y_continuous("Share", labels = percent) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("outputs/figures/appendix-figure-02.pdf"))


# -----------------------------------------------------------------------------
# 4. Gender distribution
# -----------------------------------------------------------------------------

bee |>
  ggplot() +
  geom_bar(aes(x = factor(gender), ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete("Gender") +
  scale_y_continuous("Share", labels = percent) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("outputs/figures/appendix-figure-03.pdf"))


# -----------------------------------------------------------------------------
# 5. Education level
# -----------------------------------------------------------------------------

bee |>
  mutate(education_cat = fct_relevel(education_cat, "NO", "CAP_BEPC", "BAC_BAC2", "DIPL_SUP")) |>
  ggplot() +
  geom_bar(aes(x = factor(education_cat), ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete("Education level") +
  scale_y_continuous("Share", labels = percent) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("outputs/figures/appendix-figure-04.pdf"))


# -----------------------------------------------------------------------------
# 6. Income distribution
# -----------------------------------------------------------------------------

bee |>
  mutate(
    income = case_when(
      income == 1  ~ "Less than 650 €",
      income == 2  ~ "650–950 €",
      income == 3  ~ "950–1,200 €",
      income == 4  ~ "1,200–1,400 €",
      income == 5  ~ "1,400–1,650 €",
      income == 6  ~ "1,650–1,900 €",
      income == 7  ~ "1,900–2,200 €",
      income == 8  ~ "2,200–2,500 €",
      income == 9  ~ "2,500–3,200 €",
      income == 10 ~ "More than 3,200 €",
      income > 6 & income < 7 ~ NA_character_
    ) |> fct_relevel(
      "Less than 650 €", "650–950 €", "950–1,200 €",
      "1,200–1,400 €", "1,400–1,650 €", "1,650–1,900 €",
      "1,900–2,200 €", "2,200–2,500 €", "2,500–3,200 €",
      "More than 3,200 €"
    )
  ) |>
  ggplot() +
  geom_bar(aes(x = factor(income), ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete("Income") +
  scale_y_continuous("Share", labels = percent) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("outputs/figures/appendix-figure-05.pdf"))


# -----------------------------------------------------------------------------
# 7. Urban–rural distribution
# -----------------------------------------------------------------------------

bee |>
  mutate(
    urban_rural_cat = case_when(
      urban_rural_num == 0 ~ "Rural",
      urban_rural_num == 1 ~ "Inf 19,999",
      urban_rural_num == 2 ~ "20,000–99,999",
      urban_rural_num == 3 ~ "100,000–199,999",
      urban_rural_num == 4 ~ "Agglo Paris"
    ) |> as.factor(),
    urban_rural_cat = fct_relevel(
      urban_rural_cat,
      "Rural", "Inf 19,999", "20,000–99,999", "100,000–199,999", "Agglo Paris"
    )
  ) |>
  ggplot() +
  geom_bar(aes(x = urban_rural_cat, ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete("Agglomeration size") +
  scale_y_continuous("Share", labels = percent) +
  theme_light()

ggsave(here("outputs/figures/appendix-figure-06.pdf"))


# -----------------------------------------------------------------------------
# 8. Age distribution
# -----------------------------------------------------------------------------

bee |>
  mutate(
    age_num_12 = case_when(
      age_num_12 == 4  ~ "24 and less",
      age_num_12 == 5  ~ "25–29",
      age_num_12 == 6  ~ "30–34",
      age_num_12 == 7  ~ "35–39",
      age_num_12 == 8  ~ "40–44",
      age_num_12 == 9  ~ "45–49",
      age_num_12 == 10 ~ "50–54",
      age_num_12 == 11 ~ "55–59",
      age_num_12 == 12 ~ "60–64",
      age_num_12 == 13 ~ "65–69",
      age_num_12 == 14 ~ "70–74",
      age_num_12 == 15 ~ "75 and more"
    )
  ) |>
  ggplot(aes(x = factor(age_num_12))) +
  geom_bar(aes(x = age_num_12, ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete("Age group") +
  scale_y_continuous("Share", labels = percent) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("outputs/figures/appendix-figure-07.pdf"))


# -----------------------------------------------------------------------------
# 9. Car as main transport
# -----------------------------------------------------------------------------

bee |>
  ggplot() +
  geom_bar(aes(x = factor(car_main_transport), ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete("Car as main transport") +
  scale_y_continuous("Share", labels = percent) +
  theme_light()

ggsave(here("outputs/figures/appendix-figure-08.pdf"))


# -----------------------------------------------------------------------------
# 10. Flight use
# -----------------------------------------------------------------------------

bee |>
  ggplot() +
  geom_bar(aes(x = factor(flight_use), ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete("Flight use in the last 12 months") +
  scale_y_continuous("Share", labels = percent) +
  theme_light()

ggsave(here("outputs/figures/appendix-figure-09.pdf"))


# -----------------------------------------------------------------------------
# 11. Climate concern
# -----------------------------------------------------------------------------

bee |>
  mutate(
    climate_concern = case_when(
      climate_concern == 1 ~ "Not at all concerned",
      climate_concern == 2 ~ "Not very concerned",
      climate_concern == 3 ~ "Somewhat concerned",
      climate_concern == 4 ~ "Very concerned",
      climate_concern == 5 ~ "Extremely concerned"
    )
  ) |>
  ggplot() +
  geom_bar(aes(x = factor(climate_concern), ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete("Climate concern") +
  scale_y_continuous("Share", labels = percent) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("outputs/figures/appendix-figure-10.pdf"))

message("Descriptive appendix figures (01–10) successfully exported.")

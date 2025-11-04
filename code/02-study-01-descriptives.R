# =============================================================================
# 02-study-01-descriptive.R
# Purpose  : Plot descriptive figures for appendix
# Authors  : Malo Jan
# =============================================================================
# Description:
# This script:
#   1. Imports the cleaned BEE dataset
#   2. Plots descriptive figures for appendix
# =============================================================================

# 0. Setup-------------------------------------------------------------------


library(tidyverse)
library(here)


# Load cleaned data

bee <- read_rds(here("data/processed/study-01-clean-data.rds"))

# IDEOLOGY DISTRIBTION

bee |> 
  mutate(
    ideology = case_when(
      ideology > 4 & ideology < 5 ~ NA_real_,
      .default = ideology
    )
  ) |> 
  ggplot() +
  geom_bar(aes(factor(ideology), ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() +
  scale_x_discrete('Left-right self placement')

ggsave(
  here("outputs/figures/appendix-figure-01.pdf"),
)

# Government satisfaction

bee |> 
  mutate(
  gov_sati = case_when(
    gov_sati_num == 4 ~ "Very satisfied",
    gov_sati_num == 3 ~ "Rather satisfied",
    gov_sati_num == 2 ~ "Rather disatisfied",
    gov_sati_num == 1 ~ "Very disatisfied"
  ) |> fct_relevel("Very disatisfied", "Rather disatisfied", "Rather satisfied", "Very satisfied")) |>
  ggplot() +
  geom_bar(aes(x = factor(gov_sati), ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete('Satisfaction towards government') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("outputs/figures/appendix-figure-02.pdf"))

# Gender

bee |> 
  ggplot() +
  geom_bar(aes(x = factor(gender), ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete('Gender') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("outputs/figures/appendix-figure-03.pdf"))


# Education

bee |> 
  mutate(
    education_cat = fct_relevel(education_cat, "NO", "CAP_BEPC", "BAC_BAC2", "DIPL_SUP")
  ) |> 
  ggplot() +
  geom_bar(aes(x = factor(education_cat), ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete('Education level') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("outputs/figures/appendix-figure-04.pdf"))

# Education

bee |> 
  mutate(
    income = case_when(
      income == 1  ~ "Less than 650 €",
      income ==  2  ~ "650-950 €",
      income == 3  ~ "950-1200 €",
      income ==  4  ~ "1200-1400 €",
      income ==5  ~ "1400-1650 €",
      income ==6  ~ "1650-1900 €",
      income ==7  ~ "1900-2200 €",
      income ==8  ~ "2200-2500 €",
      income ==9  ~ "2500-3200 €",
      income ==10 ~ "More than 3200 €",
      income > 6 & income < 7 ~ NA_character_) |> 
      fct_relevel("Less than 650 €", "650-950 €", "950-1200 €", "1200-1400 €", "1400-1650 €", "1650-1900 €", "1900-2200 €", "2200-2500 €", "2500-3200 €", "More than 3200 €")) |> 
  ggplot() +
  geom_bar(aes(x = factor(income), ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete('Income') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("outputs/figures/appendix-figure-05.pdf"))

# Urban rural


bee |> 
  mutate(
    urban_rural_cat = case_when(
      urban_rural_num == 0 ~ "Rural",
      urban_rural_num == 1 ~ "Inf 19999",
      urban_rural_num == 2 ~ "20000-99999",
      urban_rural_num == 3 ~ "100000-199999",
      urban_rural_num == 4 ~ "Agglo Paris") |> as.factor(),
    urban_rural_cat = fct_relevel(urban_rural_cat, "Rural", "Inf 19999", "20000-99999", "100000-199999", "Agglo Paris")) |> 
  ggplot() +
  geom_bar(aes(x = urban_rural_cat, ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete('Agglomeration size') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light()

ggsave(here("outputs/figures/appendix-figure-06.pdf"))

# Age


bee |> 
  mutate(age_num_12 = case_when(
    age_num_12 == 4 ~ "24 and less",
    age_num_12 == 5 ~ "25-29",
    age_num_12 == 6 ~ "30-34",
    age_num_12 == 7 ~ "35-39",
    age_num_12 == 8 ~ "40-44",
    age_num_12 == 9 ~ "45-49",
    age_num_12 == 10 ~ "50-54",
    age_num_12 == 11 ~ "55-59",
    age_num_12 == 12 ~ "60-64",
    age_num_12 == 13 ~ "65-69",
    age_num_12 == 14 ~ "70-74",
    age_num_12 == 15 ~ "75 and more"
  )) |> 
  ggplot(aes(x = factor(age_num_12), perc)) +
  geom_bar(aes(x = age_num_12, ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete('Age group') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() +
  # Rotate x axis
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("outputs/figures/appendix-figure-07.pdf"))
# 

bee |> 
  ggplot() +
  geom_bar(aes(x = factor(car_main_transport), ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete('Car as main transport') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() 

ggsave(here("outputs/figures/appendix-figure-08.pdf"))


bee |> 
  ggplot() +
  geom_bar(aes(x = factor(flight_use), ..prop.., group = 1, weight = POIDS_bee0)) +
  scale_x_discrete('Flight use in the last 12 months') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light()

ggsave(here("outputs/figures/appendix-figure-09.pdf"))

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
  scale_x_discrete('Climate concern') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() +
  # Rotate labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("outputs/figures/appendix-figure-10.pdf"))


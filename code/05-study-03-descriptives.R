# =============================================================================
# 05-study-03-descriptive.R
# Purpose  : Plot descriptive figures for appendix
# Authors  : Malo Jan
# =============================================================================
# Description:
# This script:
#   1. Imports the cleaned Study 3 dataset
#   2. Plots descriptive figures for appendix
# =============================================================================

# 0. Setup-------------------------------------------------------------------

library(tidyverse)
library(here)


# Load cleaned data

data <- read_rds(here("data/processed/data-study-03-clean.rds"))


data |> 
  mutate(
    ideology = case_when(
      ideology > 4 & ideology < 5 ~ NA_real_,
      .default = ideology
    )
  ) |> 
  ggplot() +
  geom_bar(aes(factor(ideology), ..prop.., group = 1, weight = weight)) +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() +
  scale_x_discrete('Left-right self placement')

ggsave(
  filename = here("outputs/figures/appendix-figure-17.png"))

data |> 
  mutate(gov_sati = gov_sati *-1 + 5) |> 
  ggplot() +
  geom_bar(aes(x = factor(gov_sati), ..prop.., group = 1, weight = weight)) +
  scale_x_discrete('Satisfaction towards government') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(
  filename = here("outputs/figures/appendix-figure-18.png"))

data |> 
  ggplot() +
  geom_bar(aes(x = factor(gender), ..prop.., group = 1, weight = weight)) +
  scale_x_discrete('Gender') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(
  filename = here("outputs/figures/appendix-figure-19.png"))
data |> 
  mutate(
    education_cat = fct_relevel(education_cat, "NO", "CAP_BEPC", "BAC_BAC2", "DIPL_SUP")
  ) |> 
  ggplot() +
  geom_bar(aes(x = factor(education_cat), ..prop.., group = 1, weight = weight)) +
  scale_x_discrete('Education level') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = here("outputs/figures/appendix-figure-20.png"))

data |> 
  mutate(
    income = case_when(
      income == 1  ~ "Less than 750 €",
      income ==  2  ~ "750-999 €",
      income == 3  ~ "1000-1499 €",
      income == 4  ~ "1500-1999 €",
      income == 5  ~ "2000-2999 €",
      income == 6  ~ "3000-3499 €",
      income == 7  ~ "3500-4999 €",
      income == 8  ~ "5000+ €", 
      is.na(income) ~ NA_character_
    ) |> 
      fct_relevel("Less than 750 €", "750-999 €", "1000-1499 €", "1500-1999 €", "2000-2999 €", "3000-3499 €", "3500-4999 €", "5000+ €")) |> 
  ggplot() +
  geom_bar(aes(x = factor(income), ..prop.., group = 1, weight = weight)) +
  scale_x_discrete('Income') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(
  filename = here("outputs/figures/appendix-figure-21.png"))
data |> 
  mutate(
    urban_rural_cat = fct_relevel(urban_rural_cat, "Rural", "Inf 19999", "20000-99999", "Sup 100000", "Agglo Paris")) |> 
  ggplot() +
  geom_bar(aes(x = urban_rural_cat, ..prop.., group = 1, weight = weight)) +
  scale_x_discrete('Agglomeration size') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light()


ggsave(
  filename = here("outputs/figures/appendix-figure-22.png"))
data |> 
  ggplot(aes(x = factor(age_num), perc)) +
  geom_bar(aes(x = age_num, ..prop.., group = 1, weight = weight)) +
  scale_x_discrete('Age group') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light()


ggsave(
  filename = here("outputs/figures/appendix-figure-23.png"))
data |> 
  ggplot() +
  geom_bar(aes(x = factor(car_main_transport), ..prop.., group = 1, weight = weight)) +
  scale_x_discrete('Car as main transport') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() 


ggsave(
  filename = here("outputs/figures/appendix-figure-24.png"))

data |> 
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
  geom_bar(aes(x = factor(climate_concern), ..prop.., group = 1, weight = weight)) +
  scale_x_discrete('Climate concern') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() +
  # Rotate labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = here("outputs/figures/appendix-figure-25.png"))

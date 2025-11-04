# =============================================================================
# 05-study-03-descriptives.R
# -----------------------------------------------------------------------------
# Purpose   : Plot descriptive figures for Appendix of Study 3
# Authors   : Malo Jan & Luis Sattelmayer
# =============================================================================
# Description:
#   This script produces descriptive statistics and plots for Study 3,
#   replicating Appendix figures. It:
#     1. Imports the cleaned Study 3 dataset
#     2. Plots key sociodemographic and attitudinal distributions
#     3. Saves figures in PDF format under `outputs/figures/`
#
# Inputs:
#   - data/processed/data-study-03-clean.rds
#
# Outputs:
#   - outputs/figures/appendix-figure-17.pdf
#   - outputs/figures/appendix-figure-18.pdf
#   - outputs/figures/appendix-figure-19.pdf
#   - outputs/figures/appendix-figure-20.pdf
#   - outputs/figures/appendix-figure-21.pdf
#   - outputs/figures/appendix-figure-22.pdf
#   - outputs/figures/appendix-figure-23.pdf
#   - outputs/figures/appendix-figure-24.pdf
#   - outputs/figures/appendix-figure-25.pdf
#
# Dependencies:
#   tidyverse, here, scales
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------


library(tidyverse)
library(here)
library(scales)


# -----------------------------------------------------------------------------
# 1. Load cleaned Study 3 data
# -----------------------------------------------------------------------------

data <- read_rds(here("data/processed/study-03-clean-data.rds"))


# -----------------------------------------------------------------------------
# 2. Ideology distribution
# -----------------------------------------------------------------------------


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

# -----------------------------------------------------------------------------
# 3. Government satisfaction
# -----------------------------------------------------------------------------


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

# -----------------------------------------------------------------------------
# 4. Gender distribution
# -----------------------------------------------------------------------------


data |> 
  ggplot() +
  geom_bar(aes(x = factor(gender), ..prop.., group = 1, weight = weight)) +
  scale_x_discrete('Gender') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = here("outputs/figures/appendix-figure-19.png"))

# -----------------------------------------------------------------------------
# 5. Education level
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# 6. Income distribution
# -----------------------------------------------------------------------------


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

# -----------------------------------------------------------------------------
# 7. Urban–rural classification
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# 8. Age distribution
# -----------------------------------------------------------------------------


data |> 
  ggplot(aes(x = factor(age_num), perc)) +
  geom_bar(aes(x = age_num, ..prop.., group = 1, weight = weight)) +
  scale_x_discrete('Age group') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light()

ggsave(
  filename = here("outputs/figures/appendix-figure-23.png"))

# -----------------------------------------------------------------------------
# 9. Car use
# -----------------------------------------------------------------------------

data |> 
  ggplot() +
  geom_bar(aes(x = factor(car_main_transport), ..prop.., group = 1, weight = weight)) +
  scale_x_discrete('Car as main transport') +
  scale_y_continuous("Share", labels = scales::percent) +
  theme_light() 

ggsave(
  filename = here("outputs/figures/appendix-figure-24.png"))

# -----------------------------------------------------------------------------
# 10. Climate concern
# -----------------------------------------------------------------------------


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

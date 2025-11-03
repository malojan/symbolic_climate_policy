# =============================================================================
# 02-study1-analysis.R
# Purpose   : Analysis for Study 1 (OLS models, tables, and figures)
# Authors   : Malo Jan & Luis Sattelmayer
# -----------------------------------------------------------------------------
# Description:
# This script reproduces the analysis for Study 1:
#   1. Imports the cleaned dataset produced by 01-study1-cleaning.R
#   2. Runs OLS models (baseline + controls)
#   3. Produces regression tables and plots of estimated effects
#   4. Tests interaction effects with behavior, ideology, and policy attitudes
#   5. Saves all tables and figures under outputs/
# Dependencies:
#   tidyverse, stargazer, broom, ggeffects
# =============================================================================

library(tidyverse)
library(stargazer)
library(broom)
library(ggeffects)
library(here)

# -----------------------------------------------------------------------------
# 0. Import data
# -----------------------------------------------------------------------------

input_file <- here("data/processed/data-study-01-clean.rds")

if (!file.exists(input_file)) {
  stop(glue::glue(
    "Data file not found: {input_file}\n",
    "Please run 01-study1-cleaning.R first to generate it."
  ))
}

bee_clean <- read_rds(input_file)

# Ensure output folders exist
dir.create(here("outputs/tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs/figures"), recursive = TRUE, showWarnings = FALSE)

# --- 1. Prepare per-experiment datasets --------------------------------------
# Helper: rename treatment/support and label experiment

prep_data <- function(data, treatment_col, support_col, experiment_name) {
  data |>
    rename(
      treatment = {{ treatment_col }},
      support   = {{ support_col }}
    ) |> 
    mutate(
    experiment = experiment_name)

}

bee_highway <- prep_data(bee_clean, treatment_highway, support_highway_num, "highway")
bee_flight  <- prep_data(bee_clean, treatment_flights, support_flights_num, "flights")

# --- 2. OLS models ------------------------------------------------------------

# Treatment-only
ols_highway_t_only <- lm(support ~ treatment, data = bee_highway, weights = POIDS_bee0)
ols_flight_t_only  <- lm(support ~ treatment, data = bee_flight,  weights = POIDS_bee0)

# With full controls
ols_highway <- lm(
  support ~ treatment + ideology + gov_sati_num + gender + income + age_num_12 +
    education_cat + urban_rural_num + car_main_transport + climate_concern,
  data = bee_highway, weights = POIDS_bee0
)

ols_flight <- lm(
  support ~ treatment + ideology + gov_sati_num + gender + income + age_num_12 +
    education_cat + urban_rural_num + flight_use + climate_concern,
  data = bee_flight, weights = POIDS_bee0
)

# --- 3. Regression Table: OLS -------------------------------------------------

model_list <- list(ols_highway_t_only, ols_highway, ols_flight_t_only, ols_flight)

stargazer(
  model_list,
  type             = "latex",
  single.row       = TRUE,
  font.size        = "small",
  digits           = 2,
  title            = "OLS models: Control only and with full controls",
  column.labels    = c("", "", "", ""),
  column.sep.width = "1pt",
  dep.var.labels   = c("Speed limit highway", "Ban air travel"),
  covariate.labels = c(
    "T- Symbolic Rich", "T- Symbolic Minister", "Ideology",
    "Government satisfaction", "Gender - Male", "Income", "Age",
    "Education - CAP/BEPC", "Education - DIPL-SUP", "Education - NO",
    "Urban - Rural", "Main transport - Car", "Flight use", "Climate concern"
  ),
  star.cutoffs     = c(0.05, 0.01, 0.001),
  omit.stat        = c("f", "ser"),
  out              = here("outputs/tables/appendix-tbl-03.tex")
)

# --- 4. Coefficient extraction for ATE plot -----------------------------------

ols_coefficients <- bind_rows(
  tidy(ols_highway, conf.int = TRUE) |> mutate(symbolic = "highway"),
  tidy(ols_flight,  conf.int = TRUE) |> mutate(symbolic = "flights")
) |>
  mutate(
    term = case_when(
      str_detect(term, "Ministers") ~ "Symbolic targeting ministers",
      str_detect(term, "Rich")      ~ "Symbolic targeting rich",
      TRUE                          ~ term
    )
  )

# --- 5. Figure 1 – ATE Plot ---------------------------------------------------

fig1 <- ols_coefficients |>
  filter(term %in% c("Symbolic targeting rich", "Symbolic targeting ministers")) |>
  ggplot(aes(estimate, symbolic, color = term)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.5)) +
  geom_label(
    aes(label = paste0(sprintf("%.2f", estimate))),
    vjust = -2, hjust = 0, size = 3,
    position = position_dodge(width = 0.5)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_light() +
  scale_color_grey("Treatment") +
  scale_x_continuous("Average Treatment Effect (ATE)") +
  theme(legend.position = "bottom") +
  scale_y_discrete("", labels = c(
    "Ban flights if train alternative < 5h",
    "Limit highway speed to 110 km/h"
  ))

ggsave(here("outputs/figures/figure-01.png"), fig1)


# --- 6. Figure 2 – Distribution of support -----------------------------------

fig2 <- bind_rows(bee_highway, bee_flight) |>
  group_by(experiment, treatment) |>
  count(support, wt = POIDS_bee0) |>
  drop_na() |>
  mutate(share = n / sum(n) * 100) |>
  mutate(
    support = case_when(
      support == 4 ~ "Totally agree",
      support == 3 ~ "Rather agree",
      support == 2 ~ "Rather disagree",
      support == 1 ~ "Totally disagree"
    ) |> fct_relevel("Totally agree", "Rather agree", "Rather disagree", "Totally disagree"),
    treatment = fct_relevel(treatment, "Control", "Ministers", "Rich"),
    experiment = case_when(
      experiment == "highway" ~ "Limit highway speed 110 km/h",
      experiment == "flights" ~ "Ban on flights if train alternative less 5hrs"
    )
  ) |>
  ggplot(aes(x = fct_reorder(support, share), share, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(
    aes(
      ymin = share - 1.96 * sqrt(share * (100 - share) / sum(n)),
      ymax = share + 1.96 * sqrt(share * (100 - share) / sum(n))
    ),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_fill_grey("Symbolic policy") +
  theme_light() +
  labs(x = "", y = "%") +
  facet_grid(~ fct_relevel(experiment, "Limit highway speed 110 km/h")) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

ggsave(here("outputs/figures/figure-02.png"), fig2)

# --- 7. Interaction Models ----------------------------------------------------

## 7.1 Interaction with behavior
ols_highway_int_behavior <- lm(
  support ~ treatment*car_main_transport + ideology + gov_sati_num + gender +
    income + age_num_12 + education_cat + urban_rural_num + climate_concern,
  data    = bee_highway,
  weights = POIDS_bee0
)

ols_flight_int_behavior <- lm(
  support ~ treatment*flight_use + ideology + gov_sati_num + gender +
    income + age_num_12 + education_cat + urban_rural_num + climate_concern,
  data    = bee_flight,
  weights = POIDS_bee0
)

stargazer(
  list(ols_highway_int_behavior, ols_flight_int_behavior),
  type             = "html",
  single.row       = TRUE,
  font.size        = "small",
  digits           = 3,
  title            = "OLS models",
  column.labels    = c("", "", "", ""),
  column.sep.width = "1pt",
  dep.var.labels   = c("Speed limit highway", "Ban air travel"),
  covariate.labels = c(
    "T- Symbolic Rich",
    "T- Symbolic Minister",
    "Main Transport - Car",
    "Flight use",
    "Ideology",
    "Government satisfaction",
    "Gender - Male",
    "Income",
    "Age",
    "Education - CAP/BEPC",
    "Education - DIPL-SUP",
    "Education - NO",
    "Urban - Rural",
    "Climate concern",
    "T- Symbolic minister: Main Transport - Car",
    "T- Symbolic Rich: Main Transport - Car",
    "T- Symbolic minister: Flight use",
    "T- Symbolic Rich: Flight use"
  ),
  ci           = FALSE,
  ci.level     = 0.95,
  flip         = TRUE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat    = c("f", "ser"),
  out          = "outputs/tables/appendix-tbl-05.tex"
)

## 7.2 Interaction with ideology
ols_highway_int_ideology <- lm(
  support ~ treatment*ideology + gov_sati_num + gender + income + age_num_12 +
    education_cat + urban_rural_num + car_main_transport + climate_concern,
  data    = bee_highway,
  weights = POIDS_bee0
)

ols_flight_int_ideology <- lm(
  support ~ treatment*ideology + gov_sati_num + gender + income + age_num_12 +
    education_cat + urban_rural_num + flight_use + climate_concern,
  data    = bee_flight,
  weights = POIDS_bee0
)

stargazer(
  list(ols_highway_int_ideology, ols_flight_int_ideology),
  type             = "html",
  single.row       = TRUE,
  font.size        = "small",
  digits           = 3,
  title            = "OLS models - Interaction between Treatment and Ideology",
  column.labels    = c("", "", "", ""),
  column.sep.width = "1pt",
  dep.var.labels   = c("Speed limit highway", "Ban air travel"),
  covariate.labels = c(
    "T- Symbolic Rich",
    "T- Symbolic Minister",
    "Ideology",
    "Government satisfaction",
    "Gender - Male",
    "Income",
    "Age",
    "Education - CAP/BEPC",
    "Education - DIPL-SUP",
    "Education - NO",
    "Urban - Rural",
    "Climate concern",
    "Main Transport - Car",
    "Flight use",
    "T- Symbolic minister: Ideology",
    "T- Symbolic Rich: Ideology"
  ),
  ci           = FALSE,
  ci.level     = 0.95,
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat    = c("f", "ser"),
  out          = "outputs/tables/appendix-tbl-04.tex"
)

## 7.3 Interaction with general climate policy support
# (Fix typo: '++' -> '+'; leaving factor labels as-is)
ols_highway_int_policy_support <- lm(
  support ~ treatment*cp_cluster + ideology + gov_sati_num + gender + income +
    age_num_12 + education_cat + urban_rural_num + car_main_transport + climate_concern,
  data    = bee_highway,
  weights = POIDS_bee0
)

ols_flight_int_policy_support <- lm(
  support ~ treatment*cp_cluster + ideology + gov_sati_num + gender + income +
    age_num_12 + education_cat + urban_rural_num + flight_use + climate_concern,
  data    = bee_flight,
  weights = POIDS_bee0
)

stargazer(
  list(ols_highway_int_policy_support, ols_flight_int_policy_support),
  type             = "html",
  single.row       = TRUE,
  font.size        = "small",
  digits           = 3,
  title            = "OLS models",
  column.labels    = c("", "", "", ""),
  column.sep.width = "1pt",
  dep.var.labels   = c("Speed limit highway", "Ban air travel"),
  covariate.labels = c(
    "T- Symbolic Rich",
    "T- Symbolic Minister",
    "Cluster - Moderate climate policy",
    "Cluster - Pro climate policy",
    "Ideology",
    "Government satisfaction",
    "Gender - Male",
    "Income",
    "Age",
    "Education - CAP/BEPC",
    "Education - DIPL-SUP",
    "Education - NO",
    "Urban - Rural",
    "Main Transport - Car",
    "Flight use",
    "Climate concern",
    "T- Symbolic Rich: Moderate climate policy",
    "T- Symbolic Minister: Moderate climate policy",
    "T- Symbolic Riche: Pro climate policy",
    "T- Symbolic Minister: Pro climate policy"
  ),
  ci           = FALSE,
  ci.level     = 0.95,
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat    = c("f", "ser"),
  out          = "outputs/tables/appendix-tbl-06.tex"
)

# 8. Predicted values plots for interactions --------------------------------

## 8.1 Interaction with behavior
predictions_ols_int_behavior <- ggpredict(
  ols_highway_int_behavior, terms = c("treatment", "car_main_transport")
) |>
  as_tibble() |>
  mutate(policy = "highway")

predictions_ols_int_behavior_flights <- ggpredict(
  ols_flight_int_behavior, terms = c("treatment", "flight_use")
) |>
  as_tibble() |>
  mutate(policy = "flights")

bind_rows(predictions_ols_int_behavior, predictions_ols_int_behavior_flights) |>
  mutate(
    x = fct_relevel(x, c("Control", "Ministers", "Rich")),
    group = case_when(
      group == "flight_never" ~ "No",
      group == "flight_more"  ~ "Yes",
      .default = group
    ),
    policy = case_when(
      policy == "flights" ~ "Ban flight if train alternative 5 hours",
      policy == "highway" ~ "Limit highway speed to 100 km/h"
    ) |> fct_relevel("Limit highway speed to 100 km/h")
  ) |>
  ggplot(aes(group, predicted, color = x)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.5)) +
  theme_light() +
  scale_color_grey(
    "", labels = c(
      "Control",
      "Symbolic policy targeting ministers",
      "Symbolic policy targeting rich"
    )
  ) +
  scale_x_discrete("Car/flight user") +
  scale_y_continuous("Predicted value of supporting costly policy") +
  theme(legend.position = "bottom") +
  facet_wrap(~policy) +
  coord_flip()

ggsave(
  "outputs/figures/appendix-figure-14.png",
)

## 8.2 Interaction with ideology
predictions_ols_int_ideology <- ggpredict(
  ols_highway_int_ideology, terms = c("treatment", "ideology[0,1,2, 3,4, 5,6,7,9, 10] ")
) |>
  as_tibble() |>
  mutate(policy = "highway")

predictions_ols_int_ideology_flights <- ggpredict(
  ols_flight_int_ideology, terms = c("treatment", "ideology[0,1,2, 3,4, 5,6,7,9, 10]")
) |>
  as_tibble() |>
  mutate(policy = "flights")

bind_rows(
  predictions_ols_int_ideology,
  predictions_ols_int_ideology_flights
) |>
  mutate(
    x = fct_relevel(x, c("Control", "Ministers", "Rich")),
    policy = case_when(
      policy == "flights" ~ "Ban flight if train alternative 5 hours",
      policy == "highway" ~ "Limit highway speed to 100 km/h"
    ) |> fct_relevel("Limit highway speed to 100 km/h")
  ) |>
  ggplot(aes(group, predicted, color = x)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 1)) +
  theme_light() +
  scale_x_discrete("Left-right self placement") +
  scale_color_grey(
    "", labels = c(
      "Control",
      "Symbolic policy targeting ministers",
      "Symbolic policy targeting rich"
    )
  ) +
  scale_y_continuous("Predicted value of supporting costly policy") +
  theme(legend.position = "bottom") +
  coord_flip() +
  facet_wrap(~policy, scales = "free")

ggsave(
  "outputs/figures/appendix-figure-15.png",
)


## 8.3 Interaction with policy support cluster

predictions_ols_int_policy_support <- ggpredict(
  ols_highway_int_policy_support, terms = c("treatment", "cp_cluster")
) |>
  as_tibble() |>
  mutate(policy = "highway")

predictions_ols_int_policy_support_flights <- ggpredict(
  ols_flight_int_policy_support, terms = c("treatment", "cp_cluster")
) |>
  as_tibble() |>
  mutate(policy = "flights")

bind_rows(
  predictions_ols_int_policy_support,
  predictions_ols_int_policy_support_flights
) |>
  mutate(
    x = fct_relevel(x, c("Control", "Ministers", "Rich")),
    group = fct_relevel(group, c("Anti-climate policy", "Moderate climate policy")),
    policy = case_when(
      policy == "flights" ~ "Ban flight if train alternative 5 hours",
      policy == "highway" ~ "Limit highway speed to 100 km/h"
    ) |> fct_relevel("Limit highway speed to 100 km/h")
  ) |>
  ggplot(aes(group, predicted, color = x)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.5)) +
  theme_light() +
  scale_color_grey(
    "", labels = c(
      "Control",
      "Symbolic policy targeting ministers",
      "Symbolic policy targeting rich"
    )
  ) +
  scale_x_discrete("", labels = c("Anti CP", "Moderate CP", "Pro CP")) +
  scale_y_continuous("Predicted value of climate policy support") +
  theme(legend.position = "bottom") +
  facet_wrap(~policy) +
  coord_flip()

ggsave(
  "outputs/figures/appendix-figure-16.png",
)

# Robustness checks - Deviation from pre-registration ----------------------- 

ols_highway_pre_registred <- lm(
  support ~ treatment + gov_sati_num + gender + income + age_num_12 +  education_cat + urban_rural_num  + car_main_transport ,
  data = bee_highway,
  weights = POIDS_bee0
)
ols_highway_paper <- lm(
  support ~ treatment + gov_sati_num + gender + income + age_num_12 +  education_cat + urban_rural_num  + car_main_transport + ideology + climate_concern,
  data = bee_highway,
  weights = POIDS_bee0
)

ols_flight_pre_registred<- lm(
  support ~ treatment + gov_sati_num + gender + income + age_num_12 +  education_cat + urban_rural_num,
  data = bee_flight,
  weights = POIDS_bee0
)
ols_flight_paper <- lm(
  support ~ treatment + gov_sati_num + gender + income + age_num_12 +  education_cat + urban_rural_num  + flight_use + ideology + climate_concern,
  data = bee_flight,
  weights = POIDS_bee0
)

stargazer(list(ols_highway_pre_registred, ols_highway_paper), 
          type = 'latex', 
          single.row = T, 
          font.size = "small",
          digits = 3,
          title = "OLS models",
          column.labels = c("As pre-registered", "Actual model in paper"),
          column.sep.width = "1pt",
          dep.var.labels = c("Speed limit higwhay"),
          ci=F, 
          covariate.labels = c(
            "T- Symbolic minister",
            "T- Symbolic Rich",
            "Government satisfaction",
            "Gender - Male",
            "Income",
            "Age",
            "Education - CAP/BEPC",
            "Education - DIPL-SUP",
            "Education - NO",
            "Urban - Rural",
            "Main transport - Car",
            "Ideology",
            "Climate concern"
          ),
          ci.level=0.95, 
          flip=TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat=c("f", "ser"),
          out = "outputs/tables/appendix-tbl-01.tex")

stargazer(ols_flight_pre_registred, ols_flight_paper, 
          type = 'latex', 
          single.row = T, 
          font.size = "small",
          digits = 3,
          title = "OLS models",
          column.labels = c("As pre-registered", "As supposed in pre-registration", "Actual model in paper"),
          column.sep.width = "1pt",
          dep.var.labels = c("Ban air travel"),
          ci=F, 
          covariate.labels = c(
            "T- Symbolic minister",
            "T- Symbolic Rich",
            "Government satisfaction",
            "Gender - Male",
            "Income",
            "Age",
            "Education - CAP/BEPC",
            "Education - DIPL-SUP",
            "Education - NO",
            "Urban - Rural",
            "Flight use",
            "Ideology",
            "Climate concern"
          ),
          ci.level=0.95, 
          flip=TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat=c("f", "ser"),
          out = "outputs/tables/appendix-tbl-02.tex")

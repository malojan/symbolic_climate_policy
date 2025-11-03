# =============================================================================
# 04-study-03-analysis.R
# Purpose  : Analysis for Study 3 (OLS models, tables, and figures)
# Authors  : Malo Jan & Luis Sattelmayer
# Date     : YYYY-MM-DD
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(tidyverse)
library(stargazer)
library(broom)
library(ggeffects)

# --- 0. Import data -----------------------------------------------------------
data <- read_rds("data/processed/data-study-03-clean.rds")

# --- 1. Prepare per-experiment datasets --------------------------------------

prep_data <- function(data, treatment_col, support_col, justice_col,
                      effective_col, seriousness_col, elite_col, experiment_name) {
  data |>
    rename(
      treatment   = {{ treatment_col }},
      support     = {{ support_col }},
      justice     = {{ justice_col }},
      effective   = {{ effective_col }},
      seriousness = {{ seriousness_col }},
      elite       = {{ elite_col }}
    ) |>
    mutate(
      experiment = experiment_name,
      treatment  = fct_relevel(
        treatment,
        "Control", "Costly + Symbolic", "Costly No Symbolic",
        "Pure Symbolic", "Symbolic No Costly"
      )
    )
}

data_highway <- prep_data(
  data,
  treatment_highway, support_highway, justice_highway,
  effective_highway, seriousness_highway, elite_highway,
  "highway"
)

data_carbon_tax <- prep_data(
  data,
  treatment_carbon_tax, support_carbon_tax, justice_carbon_tax,
  effective_carbon_tax, seriousness_carbon_tax, elite_carbon_tax,
  "carbon_tax"
)

datasets <- list(highway = data_highway, carbon_tax = data_carbon_tax)
outcomes <- c("support", "justice", "effective", "seriousness", "elite")

# --- 2. Model setup -----------------------------------------------------------

controls <- c(
  "ideology", "gov_sati", "gender", "income", "age_num",
  "education_cat", "urban_rural_cat", "car_main_transport", "climate_concern"
)

run_ols_treatment_only <- function(df, outcome) {
  f <- as.formula(paste(outcome, "~ treatment"))
  lm(f, data = df, weights = df$weight)
}

run_ols_with_controls <- function(df, outcome) {
  rhs <- paste(c("treatment", controls), collapse = " + ")
  f <- as.formula(paste(outcome, "~", rhs))
  lm(f, data = df, weights = df$weight)
}


# -----------------------------------------------------------------------------
# 4. Estimate both types efficiently
# -----------------------------------------------------------------------------

ols_models <- expand_grid(
  experiment = names(datasets),
  outcome    = outcomes,
  model_type = c("treatment_only", "with_controls")
) |>
  mutate(
    model = pmap(
      list(experiment, outcome, model_type),
      \(exp, outc, type) {
        df <- datasets[[exp]]
        if (type == "treatment_only") run_ols_treatment_only(df, outc)
        else run_ols_with_controls(df, outc)
      }
    )
  )

# --- 4. Helper for Stargazer tables (fixed) ----------------------------------
# dep_var_label  : single string for the DV name shown in the table (e.g., "Support")
# group_labels   : vector of group labels for the experiments (e.g., c("Speed limit highway","Carbon tax"))
save_stargazer <- function(df, outcome, dep_var_label, group_labels, file_name, title = NULL) {
  
  # keep models in a stable order: experiment, then model_type
  tab_df <- df |>
    filter(outcome == !!outcome,
           model_type %in% c("treatment_only", "with_controls")) |>
    arrange(experiment, model_type)
  
  # how many models in each experiment? (needed for column.separate)
  counts <- tab_df |>
    count(experiment, name = "n_models")
  
  # ensure group_labels align with the order of experiments in tab_df
  exps <- counts$experiment
  if (length(group_labels) != nrow(counts)) {
    stop("Length of 'group_labels' must match the number of experiments in the table.")
  }
  
  stargazer(
    tab_df$model,
    type = "text",
    title = title %||% paste0("OLS models Study 3 : ", stringr::str_to_title(outcome)),
    single.row = TRUE,
    font.size = "small",
    digits = 3,
    # Grouped headers: one per experiment, spanning its number of models
    column.labels   = group_labels,
    column.separate = counts$n_models,
    column.sep.width = "1pt",
    # Only ONE DV label
    dep.var.labels = dep_var_label,
    ci = FALSE,
    flip = TRUE,
    star.cutoffs = c(0.05, 0.01, 0.001),
    omit.stat = c("f", "ser"),
    covariate.labels = c(
      "T- Costly + Symbolic", "T- Costly + No Symbolic",
      "T- Symbolic only", "T- Symbolic + No Costly",
      "Ideology", "Government satisfaction", "Gender - Male",
      "Income", "Age", "Education - BAC+2", "Education - BAC+3",
      "Education - CAPBEPC", "Education - NO",
      "Urban - Rural - Agglo Paris", "Urban - Rural - Inf 19999",
      "Urban - Rural - Rural", "Urban - Rural - Sup 20000",
      "Main transport - Car", "Climate concern"
    ),
    out = file_name
  )
}

# --- 5. Export tables ---------------------------------------------------------


# --- 5. Automated export of all Study 3 tables -------------------------------
table_specs <- tribble(
  ~outcome,      ~dep_var_label,         ~title,                                      ~file,
  "support",     "Support",               "OLS models Study 3 : Policy support",       "outputs/tables/ols-models-study-03-support.tex",
  "justice",     "Fairness",              "OLS models Study 3 : Policy fairness",      "outputs/tables/ols-models-study-03-fairness.tex",
  "effective",   "Effectiveness",         "OLS models Study 3 : Policy effectiveness", "outputs/tables/ols-models-study-03-effectiveness.tex",
  "seriousness", "Seriousness",           "OLS models Study 3 : Policy seriousness",   "outputs/tables/ols-models-study-03-seriousness.tex",
  "elite",       "Elite responsiveness",  "OLS models Study 3 : Elite responsiveness", "outputs/tables/ols-models-study-03-elite.tex"
)

# Automatically export all tables
pwalk(
  table_specs,
  \(outcome, dep_var_label, title, file) {
    save_stargazer(
      df = ols_models,
      outcome = outcome,
      dep_var_label = dep_var_label,
      group_labels = c("Speed limit highway", "Carbon tax"),
      file_name = file,
      title = title
    )
  }
)


# Extract model coefficients

# -----------------------------------------------------------------------------
# 5. Tidy and label results
# -----------------------------------------------------------------------------

model_coefficients <- ols_models |>
  mutate(tidy = map(model, ~ broom::tidy(.x, conf.int = TRUE,     conf.level = 0.95
))) |>
  unnest(tidy) 

# --- 7. Figures ---------------------------------------------------------------

## Figure 3 - Treatment effects on support for highway speed limit 

model_coefficients   |> 
  filter(str_detect(term, "treatment"), 
         model_type == "with_controls") |> 
  filter(outcome == "support" & experiment != "carbon_tax") |> 
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    TRUE ~ ""),
    term = str_remove(term, "treatment") |> 
      fct_relevel(c("Symbolic No Costly", "Pure Symbolic", "Costly + Symbolic", "Costly No Symbolic"))) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5)) +
  geom_label(aes(label = paste(sprintf("%.2f", estimate), significance)), vjust = -1, hjust = 0.7, size = 2.5, position = position_dodge(width = 0.5)) +
  coord_flip() +
  scale_color_brewer(palette = "Set1") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_light() +
  scale_y_continuous("Average Treatment Effect on policy support") +
  scale_x_discrete("Treatment group")

ggsave("outputs/figures/figure-03.png")

# Figure 4 : Treatment effects on support for carbon tax

model_coefficients   |> 
  filter(str_detect(term, "treatment"), 
         model_type == "with_controls") |> 
  filter(outcome == "support" & experiment == "carbon_tax") |> 
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    TRUE ~ ""),
    term = str_remove(term, "treatment") |> 
      fct_relevel(c("Symbolic No Costly", "Pure Symbolic", "Costly + Symbolic", "Costly No Symbolic"))) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5)) +
  geom_label(aes(label = paste(sprintf("%.2f", estimate), significance)), vjust = -1, hjust = 0.7, size = 2.5, position = position_dodge(width = 0.5)) +
  coord_flip() +
  scale_color_brewer(palette = "Set1") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_light() +
  scale_y_continuous("Average Treatment Effect on policy support") +
  scale_x_discrete("Treatment group")

ggsave("outputs/figures/figure-04.png")


# Figure 5 : Treatment effects on other perceptions 
model_coefficients |> 
  filter(
    str_detect(term, "treatment"),
    model_type == "with_controls",
    outcome != "support") |> 
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    TRUE ~ ""),
    term = str_remove(term, "treatment") |> 
      fct_relevel(c("Symbolic No Costly", "Pure Symbolic", "Costly + Symbolic", "Costly No Symbolic")), 
    outcome = case_when(
      outcome == "justice" ~ "Fairness",
      outcome == "effective" ~ "Effectiveness",
      outcome == "seriousness" ~ "Seriousness",
      outcome == "elite" ~ "Elite perception"
    ),
    outcome = fct_relevel(outcome, c("Fairness", "Effectiveness", "Seriousness", "Elite perception")) )  |> 
  
  ggplot(aes(x = term, y = estimate, color = experiment)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 1)) +
  geom_text(aes(label = paste(sprintf("%.2f", estimate), significance)), vjust = -1, hjust = 1, size = 2, position = position_dodge(width = 1)) +
  coord_flip() +
  scale_color_manual("Costly policy", values = c( "#666666", "black"), labels = c("Carbon tax", "Highway speed limit")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_light() +
  scale_y_continuous("Average Treatment Effect", limits = c(-0.5, 1.3)) +
  scale_x_discrete("Treatment group") +
  facet_wrap(~ outcome) +
  # Legend at bottom
  theme(legend.position = "bottom")

ggsave("outputs/figures/figure-05.png")

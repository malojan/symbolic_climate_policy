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
library(scales)
library(here)

# --- 0. Import data -----------------------------------------------------------
data <- read_rds("data/processed/study-03-clean-data.rds")

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

# Standard deviation control group

sd_control_highway <- data_highway |> 
  filter(treatment == "Control") |> 
  summarise(
    sd_highway = sd(support, na.rm = T),
  ) |> 
  pull(sd_highway)

sd_control_carbon <- data_carbon_tax |> 
  filter(treatment == "Control") |> 
  summarise(
    sd_control_carbon = sd(support, na.rm = T),
  ) |> 
  pull(sd_control_carbon)



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
  "support",     "Support",               "OLS models Study 3 : Policy support",       "outputs/tables/appendix-tbl-08.tex",
  "justice",     "Fairness",              "OLS models Study 3 : Policy fairness",      "outputs/tables/appendix-tbl-09.tex",
  "effective",   "Effectiveness",         "OLS models Study 3 : Policy effectiveness", "outputs/tables/appendix-tbl-10.tex",
  "seriousness", "Seriousness",           "OLS models Study 3 : Policy seriousness",   "outputs/tables/appendix-tbl-11.tex",
  "elite",       "Elite responsiveness",  "OLS models Study 3 : Elite responsiveness", "outputs/tables/appendix-tbl-12.tex"
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
  unnest(tidy) |> 
  select(-model)

model_coefficients

write_rds(
  model_coefficients,
  here("data/processed/study-03-ols-coefficients.rds")
)

# --- 7. Figures ---------------------------------------------------------------

## Figure 3 - Treatment effects on support for highway speed limit 

model_coefficients   |> 
  filter(
    str_detect(term, "treatment"),
    model_type == "with_controls",
    outcome == "support",
    experiment != "carbon_tax"
  ) |>
  mutate(
    # Convert 1–4 scale to % of full range
    estimate_standardized = (estimate / 3) * 100,
    conf_low_standardized = (conf.low / 3) * 100,
    conf_high_standardized = (conf.high / 3) * 100,
    estimate_sd = case_when(
      experiment == "highway"~ estimate / 1.033991,
      experiment == "carbon_tax" ~  estimate / 0.8597786
    ),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    term = str_remove(term, "treatment") |>
      fct_relevel(c("Symbolic No Costly", "Pure Symbolic", "Costly + Symbolic", "Costly No Symbolic"))
  ) |>
  ggplot(aes(x = term, y = estimate_standardized, color = term)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_crossbar(
    aes(ymin = conf_low_standardized, ymax = conf_high_standardized),
    position = position_dodge(width = 0.6),
    linewidth = 0.8,
    width = 0.05,
    alpha = 0.9,
    show.legend = FALSE,
    
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_label(
    aes(label = paste0(sprintf("%.1f", estimate_standardized), "%", " (ATE: ", sprintf("%.2f", estimate),  ", SD: ", sprintf("%.2f", estimate_sd), ")")),
    vjust = -0.3, hjust = -0.1, size = 3,
    label.size = 0,
    fill = alpha("white", 0.7)
  )  +
  coord_flip(ylim = c(-10, 80)) +
  scale_color_manual(
    "Treatment",
    values = c(
      "Symbolic No Costly" = "#7f7f7f",   # grey baseline
      "Pure Symbolic" = "#1b9e77",        # green (matches Fig 1)
      "Costly + Symbolic" = "#d95f02",    # orange (matches Fig 1)
      "Costly No Symbolic" = "#7570b3"    # purple, distinct
    ),
    guide = "none"
  ) +
  scale_y_continuous(
    name = "Change in outcome (% of full 1–4 scale range)",
    labels = label_percent(scale = 1, accuracy = 1),
    breaks = seq(-20, 80, by = 10)   # every 5 percentage points
  ) +
  scale_x_discrete("Treatment group") +
  theme_light(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_text(size = 10, margin = margin(t = 10)),
    axis.title.x = element_text(size = 10, margin = margin(t = 10))  # smaller x-axis title
  ) 

ggsave("outputs/figures/figure-03.pdf")


# Figure 4 : Treatment effects on support for carbon tax

model_coefficients   |>
  filter(
    str_detect(term, "treatment"),
    model_type == "with_controls",
    outcome == "support",
    experiment == "carbon_tax"
  ) |>
  mutate(
    # Convert 1–4 scale to % of full range
    estimate_standardized = (estimate / 3) * 100,
    conf_low_standardized = (conf.low / 3) * 100,
    conf_high_standardized = (conf.high / 3) * 100,
    estimate_sd = case_when(
      experiment == "highway"~ estimate / 1.033991,
      experiment == "carbon_tax" ~  estimate / 0.8597786
    ),
    
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    term = str_remove(term, "treatment") |>
      fct_relevel(c("Symbolic No Costly", "Pure Symbolic", "Costly + Symbolic", "Costly No Symbolic"))
  ) |>
  ggplot(aes(x = term, y = estimate_standardized, color = term)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_crossbar(
    aes(ymin = conf_low_standardized, ymax = conf_high_standardized),
    position = position_dodge(width = 0.6),
    linewidth = 0.8,
    width = 0.05,
    alpha = 0.9,
    show.legend = FALSE,
    
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_label(
    aes(label = paste0(sprintf("%.1f", estimate_standardized), "%", " (ATE: ", sprintf("%.2f", estimate),  ", SD: ", sprintf("%.2f", estimate_sd), ")")),
    vjust = -0.3, hjust = -0.1, size = 3,
    label.size = 0,
    fill = alpha("white", 0.7)
  )  +
  coord_flip(ylim = c(0, 80)) +
  scale_color_manual(
    "Treatment",
    values = c(
      "Symbolic No Costly" = "#7f7f7f",   # grey baseline
      "Pure Symbolic" = "#1b9e77",        # green (matches Fig 1)
      "Costly + Symbolic" = "#d95f02",    # orange (matches Fig 1)
      "Costly No Symbolic" = "#7570b3"    # purple, distinct
    ),
    guide = "none"
  ) +
  scale_y_continuous(
    name = "Change in outcome (% of full 1–4 scale range)",
    labels = label_percent(scale = 1, accuracy = 1),
    breaks = seq(-20, 80, by = 10)   # every 5 percentage points
  ) +
  scale_x_discrete("Treatment group") +
  theme_light(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_text(size = 10, margin = margin(t = 10)),
    axis.title.x = element_text(size = 10, margin = margin(t = 10))  # smaller x-axis title
  )

ggsave("outputs/figures/figure-04.pdf")


# Figure 5 : Treatment effects on other perceptions 
model_coefficients |>
  filter(
    str_detect(term, "treatment"),
    model_type == "with_controls",
    outcome != "support"
  ) |>
  mutate(
    # Convert to % of full 1–4 scale range
    estimate_standardized = (estimate / 3) * 100,
    conf_low_standardized = (conf.low / 3) * 100,
    conf_high_standardized = (conf.high / 3) * 100,
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    term = str_remove(term, "treatment") |>
      fct_relevel(c("Symbolic No Costly", "Pure Symbolic", "Costly + Symbolic", "Costly No Symbolic")),
    outcome = case_when(
      outcome == "justice" ~ "Fairness",
      outcome == "effective" ~ "Effectiveness",
      outcome == "seriousness" ~ "Seriousness",
      outcome == "elite" ~ "Elite perception"
    ),
    outcome = fct_relevel(outcome, c("Fairness", "Effectiveness", "Seriousness", "Elite perception"))
  ) |>
  ggplot(aes(x = term, y = estimate_standardized, color = experiment)) +
  geom_point(position = position_dodge(width = 0.6), size = 3, ) +
  geom_crossbar(
    aes(ymin = conf_low_standardized, ymax = conf_high_standardized),
    position = position_dodge(width = 0.6),
    linewidth = 0.8,
    width = 0.05,
    alpha = 0.9,
    show.legend = FALSE,
    
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_text(
    aes(label = paste0(sprintf("%.1f", estimate_standardized), "%", significance)),
    vjust = -0.2, hjust = -0.75, size = 2,
    label.size = 0,
    fill = alpha("white", 0.7),
    position = position_dodge(width = 1),
    show.legend = FALSE,
    
  ) +
  coord_flip(ylim = c(-10, 50)) +
  scale_color_manual(
    "Treatment",
    values = c(
      "carbon_tax" = "#1b9e77",       # green
      "highway" = "#d95f02"   # orange
    ),
    labels = c(
      "Carbon Tax",
      "Highway Speed Limit"
    )
  ) +
  scale_y_continuous(
    "Change in outcome (% of full 1–4 scale range)",
    labels = scales::label_percent(scale = 1, accuracy = 1),
    limits = c(-20, 45),
    breaks = seq(-20, 40, 10)
  ) +
  scale_x_discrete("Treatment group") +
  facet_wrap(~ outcome) +
  theme_light(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "italic", size = 10),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey85"),
    strip.text = element_text(size = 11),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),
    axis.title.x = element_text(size = 10, margin = margin(t = 10))
  )

ggsave("outputs/figures/figure-05.pdf")


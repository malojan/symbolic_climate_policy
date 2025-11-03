# =============================================================================
# run_all.R
# Purpose : Master script to reproduce all analyses (replication version)
# Author  : Malo JAN
# =============================================================================

# --- 0. Ensure renv is installed ---------------------------------------------
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# --- 1. Check and activate renv environment ----------------------------------
message("Checking renv environment...")

needs_restore <- tryCatch({
  info <- renv::status()
  !identical(info$synchronized, TRUE)
}, error = function(e) TRUE)

if (needs_restore) {
  message("Environment out of sync — restoring from renv.lock ...")
  renv::restore(prompt = FALSE)
} else {
  message("renv environment up to date.")
}

# --- 2. Load base helper packages --------------------------------------------
library(purrr)
library(glue)
library(here)

message("\n===== Starting replication pipeline =====")

# --- 3. Prepare error log file -----------------------------------------------
log_file <- here::here("outputs/logs/run_all_errors.log")
if (!dir.exists(dirname(log_file))) dir.create(dirname(log_file), recursive = TRUE)
if (file.exists(log_file)) file.remove(log_file)

# Helper to append to the log file (portable version)
append_error <- function(text) {
  con <- file(log_file, open = "a")
  writeLines(text, con)
  close(con)
}

# --- 4. Function to run each script safely -----------------------------------
run_script <- function(file) {
  file_path <- here::here(file)
  message(glue("\n--- Running: {file_path} ---"))
  
  tryCatch({
    # Ensure Study 1 data exists before running cleaning
    if (grepl("01-study-01-cleaning\\.R$", file)) {
      raw_data <- here::here("data/raw/fr_cdsp_ddi_elipss_202312_bee.csv")
      if (!file.exists(raw_data)) {
        stop(glue(
          "Required data file not found: {raw_data}\n",
          "Please download it manually from https://doi.org/10.21410/7E4/OH0RKI ",
          "and place it in the 'data/raw/' folder."
        ))
      }
    }
    
    source(file_path, echo = TRUE, max.deparse.length = Inf, local = new.env())
    message(glue("Completed: {file}"))
    TRUE
    
  }, error = function(e) {
    msg <- glue("[{format(Sys.time(), '%Y-%m-%d %H:%M:%S')}] Error in {file}: {e$message}")
    message(msg)
    append_error(msg)
    FALSE
  })
}

# --- 5. Define scripts to run -------------------------------------------------
scripts <- c(
  "code/01-study-01-cleaning.R",
  "code/02-study-01-analysis.R",
  "code/03-study-03-cleaning.R",
  "code/04-study-03-analysis.R"
)

# --- 6. Execute all scripts ---------------------------------------------------
results <- purrr::map_lgl(scripts, run_script)

# --- 7. Summary ---------------------------------------------------------------
n_success <- sum(results)
n_total   <- length(results)

message(glue("\n===== Replication pipeline finished ({n_success}/{n_total} scripts ran successfully) ====="))

if (n_success < n_total) {
  message(glue("Some scripts failed. Check log: {log_file}"))
} else {
  message("All scripts completed successfully.")
}

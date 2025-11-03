# =============================================================================
# run_all.R
# Purpose : Master script to reproduce all analyses (replication version)
# Author  : Malo JAN
# =============================================================================

# --- 0. Ensure renv is installed ---------------------------------------------
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# Activate renv (usually already done via .Rprofile)
message("Checking renv environment...")

# Check if environment is out of sync or missing
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

message(glue("\n===== Starting replication pipeline ====="))

# --- 3. Helper to safely run scripts ------------------------------------------
run_script <- function(file) {
  file_path <- here::here(file)
  message(glue("\n--- Running: {file_path} ---"))
  tryCatch({
    source(file_path, echo = TRUE, max.deparse.length = Inf, local = new.env())
    message(glue("Completed: {file}"))
    TRUE
  }, error = function(e) {
    message(glue("Error in {file}: {e$message}"))
    FALSE
  })
}

# --- 4. Define scripts to run -------------------------------------------------
scripts <- c(
  "code/01-study-01-cleaning.R",
  "code/02-study-01-analysis.R",
  "code/03-study-03-cleaning.R",
  "code/04-study-03-analysis.R"
)

# --- 5. Execute pipeline ------------------------------------------------------
results <- purrr::map_lgl(scripts, run_script)

# --- 6. Summary ---------------------------------------------------------------
n_success <- sum(results)
n_total   <- length(results)
message(glue("\n===== Replication pipeline finished ({n_success}/{n_total} scripts ran successfully) ====="))

# =============================================================================
# run_all.R
# Purpose : Master script to reproduce all analyses (robust renv-safe version)
# Author  : Malo JAN
# =============================================================================

# --- 0. Ensure renv is available ---------------------------------------------
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# Activate renv only if not already active
if (is.null(Sys.getenv("RENV_PROJECT")) || Sys.getenv("RENV_PROJECT") == "") {
  tryCatch({
    renv::activate()
    message("renv environment activated.")
  }, error = function(e) {
    message(paste("renv activation failed:", e$message))
    stop("Cannot continue without renv environment.")
  })
} else {
  message("renv already active — skipping activation.")
}

# --- 1. Load libraries (after renv activation) -------------------------------
for (pkg in c("purrr", "glue", "here")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

message(glue("\n===== Starting replication pipeline ====="))

# --- 2. Set working directory -------------------------------------------------
root_dir <- here::here()
setwd(root_dir)
message(glue("📁 Working directory set to: {root_dir}"))

# --- 3. Helper function -------------------------------------------------------
run_script <- function(file) {
  message(glue("\n--- Running: {file} ---"))
  tryCatch(
    {
      source(file, echo = TRUE, max.deparse.length = Inf)
      message(glue("Completed: {file}"))
      TRUE
    },
    error = function(e) {
      message(glue("Error in {file}: {e$message}"))
      FALSE
    }
  )
}

# --- 4. Define scripts --------------------------------------------------------
scripts <- c(
  "code/01-study-01-cleaning.R",
  "code/02-study-01-analysis.R",
  "code/03-study-03-cleaning.R",
  "code/04-study-03-analysis.R"
)

# --- 5. Run everything --------------------------------------------------------
results <- purrr::map_lgl(scripts, run_script)

# --- 6. Summary ---------------------------------------------------------------
n_success <- sum(results)
n_total   <- length(results)
message(glue("\n===== Replication pipeline finished ({n_success}/{n_total} scripts ran successfully) ====="))

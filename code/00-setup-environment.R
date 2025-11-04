# =============================================================================
# 00-setup-environment.R
# Purpose  : Set up the R environment using renv for the project
# Authors  : Malo Jan
# =============================================================================

# --- 1. Ensure renv is installed ---------------------------------------------

if (!requireNamespace("renv", quietly = TRUE)) {
  message("Installing 'renv' package ...")
  install.packages("renv")
}


# --- 2. Check and activate renv environment ---------------------------------

message("Checking renv environment...")

needs_restore <- tryCatch({
  info <- renv::status()
  !identical(info$synchronized, TRUE)
}, error = function(e) {
  # If renv::status() fails (e.g., first run), force restore
  TRUE
})

if (needs_restore) {
  message("Environment out of sync — restoring from renv.lock ...")
  renv::restore(prompt = FALSE)
} else {
  message("renv environment up to date.")
}

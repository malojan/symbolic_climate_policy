
#  Ensure renv is installed ---------------------------------------------
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
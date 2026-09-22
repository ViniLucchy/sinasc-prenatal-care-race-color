required <- c("tidyverse","readxl","janitor","here","sandwich","lmtest","broom","binom","gt","writexl","renv")
missing <- setdiff(required, rownames(installed.packages()))
if (length(missing) > 0) install.packages(missing)
if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE)
  renv::snapshot()
}
message("Setup complete: ", R.version.string)

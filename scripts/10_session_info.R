library(here)
sink(here("outputs","logs","sessionInfo.txt"));print(sessionInfo());sink()
if(requireNamespace("renv",quietly=TRUE)) renv::snapshot(prompt=FALSE)
message("Saved session information and renv.lock.")

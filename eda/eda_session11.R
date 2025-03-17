library(tidyverse)
library(dplyr)

# First step: Understanding data's basic structure

session5 <- readRDS("../Data/session5.rds")
session5.spks11 <- session5$spks[[11]]
session5.time11 <- session5$time[[11]]

cat("Session 5 variables:", names(session5), "\n")
cat("Session 5 num. trials:", length(session5$feedback_type), "\n")

cat("Session 5, 11th trial\n")
cat("Mouse name:", session5$mouse_name, "\n")
cat("Feedback (outcomes):", session5$feedback_type[[11]], "\n")
cat("Contrast left:", session5$contrast_left[[11]], "\n")
cat("Contrast right:", session5$contrast_right[[11]], "\n")
cat("Brain area:", session5$brain_area[[11]], "\n")

cat("Spikes dimensionality:", dim(session5.spks11), "\n")
cat("Time length:", length(session5.time11), "\n")
cat("Brain area length:", (length(session5$brain_area)), "\n\n")

# Spikes are time-indexed
for (i in 1:10) {
  cat("Time ", i, ": ", session5.time11[[i]], '\n', sep="")
  cat("Spikes: ", session5.spks11[[i]], '\n', sep="")
}

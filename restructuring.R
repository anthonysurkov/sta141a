library(dplyr)
library(tidyverse)


# Objective: to have a data structure that is anything other than 4 levels of 
#            lists


# Original data structure:
# "sessions" is a list of sessions.
# each session is a list of trials. It has metadata: the mouse name and 
# experiment date.
# each trial is a list of variables about the trial. It has metadata: the L/R 
# contrast and feedback and...
# - a list of vectors 1:40 that encode the states of every k neurons
# - a list of timepoints 1:40
# - a list of k brain areas for each neuron

# Restructuring (escaping base R...):
# Every trial should be a k x 40 tibble, with k neurons on the rows and 40 
# columns.
# It can also include the brain areas, feedback_type, and L/R contrast into 
# the first columns.
# Then, every session can be a list of these dataframes.


# session_to_tibbles()
# @param: session; neural data loaded from .rds files under Data
# @returns: a list of tibbles encoding the information of each trial in the 
#          session
session_to_tibbles <- function(session) {
  trials <- lapply(seq_along(session$spks), function(i) {
    t <- tibble(
      neuron_id = seq_len(nrow(session$spks[[i]])),
      region = session$brain_area,
      feedback = session$feedback_type[[i]],
      contrast_left = session$contrast_left[[i]],
      contrast_right = session$contrast_right[[i]],
      mouse_name = session$mouse_name,
      as_tibble(session$spks[[i]])
    )
    
    return(t)
  })
  
  trials <- lapply(trials, function(trial) {
    colnames(trial)[7:46] <- paste0('t', 1:40)
    return(trial)
  })
  
  return(trials)
}

# load_sessions()
# @param: n/a
# @returns: list of sessions loaded from .rds files in Data, converted to lists
#          of tibbles
load_sessions <- function() {
  sessions <- list()
  for (i in 1:18) {
    sessions[[i]] <- readRDS(paste("../Data/session", i, ".rds", sep=""))
  }
  
  # Explicitly remove session 1 & 18 because they are used for validation data
  sessions <- sessions[-c(1,18)]
  
  sessions <- lapply(sessions, function(s) session_to_tibbles(s))
  return(sessions)
}

# load_validation_sessions()
# @param: n/a
# @returns: list with test data 1, 2
load_validation_sessions <- function() {
  sessions <- list()
  sessions[[1]] <- readRDS("../Data/test1.rds")
  sessions[[2]] <- readRDS("../Data/test2.rds")
  sessions <- lapply(sessions, function(s) session_to_tibbles(s))
  return(sessions)
}
  
# load_unified_sessions()
# @param: n/a
# @returns: a single list of all trials from all sessions as tibbles
load_unified_sessions <- function() {
  sessions <- load_sessions()
  unified <- unlist(sessions, recursive=FALSE)
  return(unified)
}

# print_yn_counts(trials)
# @param: trials; list of trials (as tibbles) of interest
# @returns: n/a
print_yn_counts <- function(trials) {
  keep(trials, ~ .x$feedback[[1]] == 1) %>% length() %>% print()
  keep(trials, ~ .x$feedback[[1]] == -1) %>% length() %>% print()
}

# equalize_yn_trials(trials)
# @param: trials; list of trials (as tibbles) of interest
# @returns: trials list augmented with 2x more "n" trials, injected with
#           random noise
equalize_yn_trials <- function(trials) { # need ~2x more "n" trials
  
  # This is the (wrong) equalization used in all my models. Please see
  # commented-out section below for alternative solution.
  trials.n <- keep(trials, ~ .x$feedback[[1]] == -1) 
  trials.y <- keep(trials, ~ .x$feedback[[1]] == 1)

  trials.n.addtl <- length(trials.y) - length(trials.n)
  
  noise <- 0.08
  times <- paste0("t", 1:40)
  
  trials.n.addtl <- lapply(trials.n.addtl, function(t) {
    t <- t %>% 
      mutate(across(all_of(times), ~ ifelse(runif(n()) < noise, 1 - ., .)))
    return(t)
  })
  
  trials.n.aug <- c(trials.n, trials.n.addtl)
  trials <- c(trials.y, trials.n.aug)
   
  # Alternative solution that resulted in "true" model training (or lack
  # thereof)
  #trials.y <- trials.y %>% sample(size=length(trials.n))
  
  #trials <- c(trials.y, trials.n)
  #trials <- sample(trials) # shuffle
  #return(trials)
}

# gen_train_val_sets(trials, train_or_val)
# @param: trials; list of trials (as tibbles) of interest
#         train_or_val=1 returns training data;
#         train_or_val=2 returns validation data;
# @returns: separated training or validation data
gen_train_val_sets <- function(trials, train_or_val=1) {
  trials.n <- keep(trials, ~ .x$feedback[[1]] == -1)
  trials.y <- keep(trials, ~ .x$feedback[[1]] == 1)
  
  len.n <- length(trials.n)
  len.y <- length(trials.y)
  
  train.n.idx <- sample(seq_along(trials.n), size=floor(0.9 * len.n))
  train.n <- trials.n[train.n.idx]
  valid.n <- trials.n[-train.n.idx]
  
  train.y.idx <- sample(seq_along(trials.y), size=floor(0.9 * len.y))
  train.y <- trials.y[train.y.idx]
  valid.y <- trials.y[-train.y.idx]
  
  train <- c(train.y, train.n)
  valid <- c(valid.y, valid.n)
  
  train_valid = list(train, valid)
  return(train_valid)
}

# restructuring_test(): validation of trial augmentation procedure
# @param; n/a
# @returns; n/a
restructuring_test <- function() {
  set.seed(123)
  
  trials <- load_unified_sessions()
  print_yn_counts(trials)
  
  trials <- equalize_yn_trials(trials)
  print_yn_counts(trials)
  
  train <- gen_train_val_sets(trials, 1)
  valid <- gen_train_val_sets(trials, 2)
  print("yn_counts train:")
  print_yn_counts(train)
  print("yn_counts valid:")
  print_yn_counts(valid)
}  

#restructuring_test()

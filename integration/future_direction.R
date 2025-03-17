library(tidyverse)
library(dplyr)
library(purrr)

#source("restructuring.R") # run me to get functions

trials <- load_unified_sessions()
min.nrow <- min(map_int(trials, nrow))

pos_indices <- get_pos_indices(d=40)
pos_indices <- pos_indices %>%
  pivot_longer(
    cols=-time,
    values_to="val"
  ) %>%
  pivot_wider(
    names_from=time,
    values_from=val
  )

trials <- lapply(trials, function(trial) {
  if (nrow(trial) > min.nrow) {
    trial %>% slice_sample(n = min.nrow)
  }
  trial <- trial %>% 
    select(feedback, contrast_left, contrast_right, t1:t40)
  
  pos_indices <- pos_indices %>% 
    mutate(feedback=trial$feedback[[1]], 
           row_count=nrow(pos_indices))
  
  trial <- bind_rows(trial, pos_indices)
  
  return(trial)
})

trials <- equalize_yn_trials(trials)
train_valid <- gen_train_val_sets(trials)
train.data <- train_valid[[1]]
valid.data <- train_valid[[2]]

train.matrices <- Filter(function(x) nrow(x) >= 40, train.data)
valid.matrices <- Filter(function(x) nrow(x) >= 40, valid.data)

# Convert to tensor-like object
train.dims <- dim(train.matrices[[1]])
train.array <- array(unlist(train.matrices), 
                     dim=c(train.dims[1],
                           train.dims[2],
                           length(train.matrices)))
train.array <- aperm(train.array, c(3,1,2))

valid.dims <- dim(valid.matrices[[1]])
valid.array <- array(unlist(valid.matrices), 
                     dim=c(valid.dims[1],
                           valid.dims[2],
                           length(valid.matrices)))
valid.array <- aperm(valid.array, c(3,1,2))

# Confirm dimensions
print(dim(train.array))
print(dim(valid.array))

# Export
np <- import("numpy")
np$save("../Data/weird_idea.npy", train.array)
np$save("../Data/weird_idea.npy", valid.array)

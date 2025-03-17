library(dplyr)
library(tidyverse)
library(moments)
library(zoo)
library(robustbase)
library(reticulate)

source("./restructuring.R")


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# My data architecture:                                                       #
#                                                                             #
# A k x 40 matrix, where...                                                   #
# - 40 features are a sine/cosine index preserving temporal structure;        #
# - k-40 features are handcrafted summary statistics;                         #
#                                                                             #
# Ablation studies can help learn the optimal value of x and the utility of   #
# every kth feature.                                                          #
#                                                                             #
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


# get_spks(trials)
# @param: trials; all trials (as tibbles) of interest
# @returns: all trials' spks data bound together into one tibble
get_spks <- function(trials) {
  trials.spks <- lapply(trials, function(t) t %>% select(t1:t40))
  trials.spks <- bind_rows(trials.spks)
  return(trials.spks)
}

# Positional encoding
#
# Source: https://medium.com/@hunter-j-phillips/positional-encoding-7a93db4109e6
# For each k = 0 to L - 1:
#   For each i = 0 to d_{model} / 2:
#     PE(k,2i) = sin(k/n ^ (2i / d_{model}))
#     PE(k,2i+1) = cos(k/n ^ /(2i / d_{model}))
#
# sine_cos_index(trials_len): generates sine-cos indices to preserve temporal
#                             information.
# @param: L; number of tokens, all models have 40 by default
#         d; number of features; variable per model
#         n; any constant, though literature recommends n=10,000
# @returns: sine-cos indices as an L x d tibble
get_pos_indices <- function(L=40, d, n=10000) {
  if (d %% 2 != 0) stop("d must be even")
 
  pos <- matrix(0, nrow=L, ncol=d)
  
  for (k in 1:L) {
    for (i in 1:(d/2)) {
      # formula corrected for R's 1-indexing
      pos[k, 2*i-1]  <- sin((k-1) / n ^ ((2 * i) / d))
      pos[k, 2*i] <- cos((k-1) / (n ^ (2 * i) / d))
    }
  }
  pos <- pos %>% 
         as_tibble() %>% 
         mutate(time = paste0('p', 1:40)) %>% 
         relocate(time)
  
  return(pos)
}

# get_means(trials, name)
# @param: trials; all trials (as tibbles) of interest
#         name; name of means column in returned tibble
# @returns: 40x2 tibble of timebin-indexed average activations
get_means <- function(trials, name="mean_overall") {
  trials.spks <- get_spks(trials)
  
  means <- colMeans(trials.spks)
  t <- enframe(means, name="time", value=name)
  return(t)
}

# get_medians(trials, name)
# @param: trials; all trials (as tibbles) of interest
#         name; name of medians column in returned tibble
# @returns: 40x2 tibble of timebin-indexed average activations
get_medians <- function(trials, name="median_overall") {
  trials.spks <- get_spks(trials)
  
  medians <- apply(trials.spks, 2, median)
  t <- enframe(medians, name="time", value=name)
  return(t)
}

# get_stdevs(trials, name)
# @param: trials; all trials (as tibbles) of interest
#         name; name of stdev column in returned tibble
# @returns: 40x2 tibble of timebin-indexed average activations
get_stdevs <- function(trials, name="sd_overall") {
  trials.spks <- get_spks(trials)
  
  sds <- apply(trials.spks, 2, sd)
  t <- enframe(sds, name="time", value=name)
  return(t)
}

# get_skewness(trials, name)
# @param: trials; all trials (as tibbles) of interest
#         name; name of skewness column in returned tibble
# @returns: 40x2 tibble of timebin-indexed activation skewness
get_skewness <- function(trials, name="skewness_overall") {
  trials.spks <- get_spks(trials)
  
  skew <- skewness(trials.spks)
  t <- enframe(skew, name="time", value=name)
  return(t)
}

# get_kurtosis(trials, name)
# @param: trials; all trials (as tibbles) of interest
#         name; name of kurtosis column in returned tibble
# @returns: 40x2 tibble of timebin-indexed activation kurtosis
get_kurtosis <- function(trials, name="kurtosis_overall") {
  trials.spks <- get_spks(trials)
  
  kurt <- kurtosis(trials.spks)
  t <- enframe(kurt, name="time", value=name)
  return(t)
}

# get_first_deriv(trials, name)
# @param: trials; all trials (as tibbles) of interest
#         name; name of first derivative column in returned tibble
# @returns: 40x2 tibble of timebin-indexed activation first derivatives
get_first_deriv <- function(trials, name="first_derivative") {
  trials.spks <- get_spks(trials)
  
  trials.1derivs <- t(apply(trials.spks, 1, diff))
  trials.avg.1derivs <- colMeans(trials.1derivs)
  trials.avg.1derivs <- c(0, trials.avg.1derivs)
  
  t <- enframe(trials.avg.1derivs, name="time", value=name)
  return(t)
}

# get_second_deriv(trials, name)
# @param: trials; all trials (as tibbles) of interest
#         name; name of second derivative column in returned tibble
# @returns: 40x2 tibble of timebin-indexed activation second derivatives
get_second_deriv <- function(trials, name="second_derivative") {
  trials.spks <- get_spks(trials)
  
  trials.2derivs <- t(apply(trials.spks, 1, function(x) {
    diff(diff(x))
  }))
  trials.avg.2derivs <- colMeans(trials.2derivs)
  trials.avg.2derivs <- c(0, 0, trials.avg.2derivs)
  
  t <- enframe(trials.avg.2derivs, name="time", value=name)
  return(t)
}

# get_rolling_mean(trials, window, name)
# @param: trials; all trials (as tibbles) of interest
#         window; size of window for rolling mean
#         name; name of rolling means column in returned tibble
# @returns: 40x2 tibble of timebin-indexed rolling activation means
get_rolling_mean <- function(trials, window=3, name="mean_rolling") {
  trials.spks <- get_spks(trials)
  
  means <- colMeans(trials.spks)
  rolling.means <- rollapply(means, width=window, 
                             FUN=mean, align="right", fill=0, partial=TRUE)
  t <- enframe(rolling.means, name="time", value=name)
  return(t)
}

# get_rolling_median(trials, window, name)
# @param: trials; all trials (as tibbles) of interest
#         window; size of window for rolling median
#         name; name of rolling means column in returned tibble
# @ returns: 40x2 tibble of timebin-indexed rolling activation medians
get_rolling_median <- function(trials, window=3, name="median_rolling") {
  trials.spks <- get_spks(trials)
  
  medians <- apply(trials.spks, 2, median)
  rolling.medians <- rollapply(medians, width=window,
                               FUN=mean, align="right", fill=0, partial=TRUE)
  t <- enframe(rolling.medians, name="time", value=name)
  return(t)
}

# get_rolling_sd(trials, window, name)
# @param: trials; all trials (as tibbles) of interest
#         window; size of window for rolling stdev
#         name; name of rolling stdev column in returned tibble
# @returns: 40x2 tibble of timebin-indexed rolling stdev means
get_rolling_sd <- function(trials, window=3, name="sd_rolling") {
  trials.spks <- get_spks(trials)
  sds <- apply(trials.spks, 2, sd)
  rolling.sds <- rollapply(sds, width=window, 
                           FUN=mean, align="right", fill=0)
  t <- enframe(rolling.sds, name="time", value=name)
  return(t)
  }

# get_rolling_skew(trials, window, name)
# @param: trials; all trials (as tibbles) of interest
#         window; size of window for rolling skewness
#         name; name of rolling skewness column in returned tibble
# @returns: 40x2 tibble of timebin-index rolling skewness means
get_rolling_skew <- function(trials, window=3, name="skew_rolling") {
  trials.spks <- get_spks(trials)
  skews <- apply(trials.spks, 2, skewness)
  rolling.skews <- rollapply(skews, width=window, 
                             FUN=mean, align="right", fill=0)
  t <- enframe(rolling.skews, name="time", value=name)
  return(t)
}

# get_rolling_kurtosis(trials, window, name)
# @param: trials; all trials (as tibbles) of interest
#         window; size of window for rolling kurtosis
#         name; name of rolling kurtosis column in returned tibble
# @returns: 40x2 tibble of timebin-indexed rolling kurtosis means
get_rolling_kurtosis <- function(trials, window=3, name="kurtosis_rolling") {
  trials.spks <- get_spks(trials)
  kurts <- apply(trials.spks, 2, kurtosis)
  rolling.kurts <- rollapply(kurts, width=window, 
                             FUN=mean, align="right", fill=0)
  t <- enframe(rolling.kurts, name="time", value=name)
  return(t)
}

# get_rolling_firstderiv(trials, window, name)
# @param: trials; all trials (as tibbles) of interest
#         window; size of window for first derivative
#         name; name of rolling first derivative column in returned tibble
# @returns: 40x2 tibble of timebin-indexed first derivative means
get_rolling_firstderiv <- function(trials, window=3, name="firstder_rolling") {
  firstderv <- get_first_deriv(trials)
  rolling.firstderv <- rollapply(firstderv[,2], width=window,
                                 FUN=mean, align="right", fill=0)
  t <- enframe(rolling.firstderv, name="time", value=name)
  return(t)
}

# get_rolling_secondderiv(trials, window, name)
# @param: trials; all trials (as tibbles) of interest
#         window; size of window for second derivative
#         name; name of rolling second derivative column in returned tibble
# @returns: 40x2 tibble of timebin-indexed second derivative means
get_rolling_secondderiv <- function(trials, window=3, name="secder_rolling") {
  secondderv <- get_second_deriv(trials)
  rolling.secondderv <- rollapply(secondderv[,2], width=window,
                                  FUN=mean, align="right", fill=0)
  t <- enframe(rolling.secondderv, name="time", value=name)
  return(t)
}

# get_regions(trial)
# @param: trial; a trial (as tibble) of interest
# @returns: list of all brain regions present in the trial
get_regions <- function(trial) {
  trial.regions <- trial$region %>% unlist() %>% unique()
  return(trial.regions)
}

# separate_spks_region(trial, regions)
# @param: trial; a trial (as tibble) of interest
#         regions; all regions associated with a trial, found by get_regions()
# @returns: all activation data sorted by region
separate_spks_region <- function(trial, regions) {
  spks.regional <- lapply(regions, function(r) {
    spks <- trial %>%
      filter(region == r) %>%
      select(t1:t40)
    return(spks) 
  })
  return(spks.regional)
}

# get_regional_autocorr(trial)
# @param: trial; a trial (as tibble) of interest
# @returns: autocorrelation matrices (n x 40), where n is the number of brain
#           regions
get_regional_autocorr <- function(trial) {
  regions <- get_regions(trial)
  spks.regional <- separate_spks_region(trial, regions)
  
  acfs.regional <- lapply(spks.regional, function(spks) {
    spks.regional.means <- colMeans(spks)
    acf.data <- acf(spks.regional.means, lag.max=40, plot=FALSE)$acf
    acf.data[1] <- 0
    acf.data <- acf.data[1:40]
    return(acf.data)
  })
  
  acf.matrix <- do.call(rbind, acfs.regional)
  colnames(acf.matrix) <- paste0("lag_", 1:ncol(acf.matrix))
  acf.t <- as.tibble(acf.matrix)
  
  return(acf.t)
}

# get_avg_autcorr(autocorrelations)
# @param: autocorrelations; all autocorrelations (as tibble) for trial of 
#         interest
# @returns: 40-vector of average autocorrelations for a trial
get_avg_autocorr <- function(autocorrelations) {
  return(colMeans(autocorrelations))
}

# get_sd_autocorr(autocorrelations)
# @param: autocorrelations; all autocorrelations (as tibble) for trial of
#         interest
# @returns: 40-vector of standard derivation of autocorrelations for a trial
get_sd_autocorr <- function(autocorrelations) {
  autocorr.sd <- apply(autocorrelations, 2, sd)
  return(autocorr.sd)
}

# get_skew_autocorr(autocorrelations)
# @param: autocorrelations; all autocorrelations (as tibble) for trial of
#         interest
# @returns: 40-vector of skewness of autocorrelations for a trial
get_skew_autocorr <- function(autocorrelations) {
  autocorr.skew <- apply(autocorrelations, 2, skewness)
  return(autocorr.skew)
}

# get_kurt_autocorr(autocorrelations)
# @param: autocorrelations; all autocorrelations (as tibble) for a trial of
#         interest
# @returns: 40-vector of kurtosis of autocorrelations for a trial
get_kurt_autocorr <- function(autocorrelations) {
  autocorr.kurt <- apply(autocorrelations, 2, kurtosis)
  return(autocurr.kurt)
}

# get_deriv1_autocorr(autocorrelations)
# @param: autocorrelations; all autocorrelations (as tibble) for a trial of
#         interest
# @returns: 40-vector of first derivative of autocorrelations for a trial
get_deriv1_autocorr <- function(autocorrelations) {
  autocorr.deriv1 <- apply(autocorrelations, 2, function(x) {
    mean(diff(x))
  })
  return(autocorr.deriv1)
}

# get_deriv2_autocorr(autocorrelations)
# @param: autocorrelations; all autocorrelations (as tibble) for a trial of
#         interest
# @returns: 40-vector of second derivative of autocorrelations for a trial
get_deriv2_autocorr <- function(autocorrelations) {
  autocorr.deriv2 <- apply(autocorrelations, 2, function(x) {
    mean(diff(diff(x)))
  })
  return(autocorr.deriv2)
}

# get_autocorr_eigenvalues(autocorrelations)
# @param: autocorrelations; all autocorrelations (as tibble) for a trial of
#         interest
# @returns: top 4 eigenvalues of a correlation matrix generated from the
#           autocorrelations for a trial
get_autocorr_eigenvalues <- function(autocorrelations) {
  autocorrelations <- na.omit(autocorrelations)
  
  corr.matrix <- cor(t(autocorrelations), use="pairwise.complete.obs")
  eigen <- eigen(corr.matrix)
  top.4 <- eigen$values[1:4]
  return(top.4)
}

# get_rolling_autocorr_eigens(autocorrs, window)
# @param: autocorrelations; all autocorrelations (as tibble) for a trial of
#         interest
#         window; size of window for eigenvalue calculation
# @returns: 4x40 tibble of 4 eigenvalues across 40 timepoints
get_rolling_autocorr_eigens <- function(autocorrs, window=7) {
  autocorrs <- na.omit(autocorrs)
  
  ac.matrix <- as.matrix(autocorrs)
  ac.matrix.t <- t(ac.matrix)
  
  rolling.eigenvalues <- rollapply(data=ac.matrix.t, width=window, 
    FUN=function(autocorr.chunk) {
      # check for rolling window of size 1; breaks correlation matrix
      if (is.null(dim(autocorr.chunk))) {
        autocorr.chunk <- matrix(autocorr.chunk, nrow=1)
      }
      if (nrow(autocorr.chunk) < 2) return(rep(0,4))
      
      if (any(apply(autocorr.chunk, 2, sd, na.rm=TRUE) == 0)) {
        return(rep(0,4))
      }
      
      corr.matrix <- cor(autocorr.chunk, use="pairwise.complete.obs")
      eigen <- eigen(corr.matrix)
      top.4 <- eigen$values[1:4]
      return(top.4)
      
  }, by.column=FALSE, align="right", fill=NA, partial=TRUE)
  
  colnames(rolling.eigenvalues) <- paste0(window, "_eig", 1:4)
  timepoints <- paste0("t", 1:40)
  
  rolling.tib <- as.tibble(rolling.eigenvalues) %>%
    mutate(time = timepoints) %>%
    select(time, everything())
  
  return(rolling.tib)
}

# mine_data(trials): generates transformer-ready tibble
# @param: trials; all trials (as tibbles)
# @returns: (40) x (k+d) tibble of data, where...
#           - d := encoding size, typically d=40 (one positional d-vector per 
#             timepoint)
#           - k := number of handmade features
mine_data <- function(trials) {
  transformer.ready <- lapply(seq_along(trials), function(i) {
    #if (i %% 100 == 0) {
    print(paste("Processing trial", i, "of", length(trials), sep=" ")) 
    #}
    
    trial <- list(trials[[i]])
    
    # Handmade features
    means <- get_means(trial, "mean_overall")
    medians <- get_medians(trial, "median_overall")
    sd <- get_stdevs(trial, "sd_overall")
    skew <- get_stdevs(trial, "skew_overall")
    kurt <- get_stdevs(trial, "kurt_overall")
    first_deriv <- get_first_deriv(trial, "firstderiv_overall")
    second_deriv <- get_second_deriv(trial, "secondderiv_overall")
    means.roll3 <- get_rolling_mean(trial, 3, "means_roll3")
    means.roll7 <- get_rolling_mean(trial, 7, "means_roll7")
    means.roll10 <- get_rolling_mean(trial, 10, "means_roll10")
    medians.roll3 <- get_rolling_median(trial, 3, "medians_roll3")
    medians.roll7 <- get_rolling_median(trial, 7, "medians_roll7")
    medians.roll10 <- get_rolling_median(trial, 10, "medians_roll10")
    sd.roll3 <- get_rolling_sd(trial, 3, "sd_roll3")
    sd.roll7 <- get_rolling_sd(trial, 7, "sd_roll7")
    sd.roll10 <- get_rolling_sd(trial, 10, "sd_roll10")
    skew.roll3 <- get_rolling_skew(trial, 3, "skew_roll3")
    skew.roll7 <- get_rolling_skew(trial, 7, "skew_roll7")
    skew.roll10 <- get_rolling_skew(trial, 10, "skew_roll10")
    kurt.roll3 <- get_rolling_kurtosis(trial, 3, "kurt_roll3")
    kurt.roll7 <- get_rolling_kurtosis(trial, 7, "kurt_roll7")
    kurt.roll10 <- get_rolling_kurtosis(trial, 10, "kurt_roll10")
    der1.roll3 <- get_rolling_firstderiv(trial, 3, "der1_roll3")
    der1.roll7 <- get_rolling_firstderiv(trial, 7, "der1_roll7")
    der1.roll10 <- get_rolling_firstderiv(trial, 10, "der1_roll10")
    der2.roll3 <- get_rolling_secondderiv(trial, 3, "der2_roll3")
    der2.roll7 <- get_rolling_secondderiv(trial, 7, "der2_roll7")
    der2.roll10 <- get_rolling_secondderiv(trial, 10, "der2_roll10")
 
    autocorr.reg <- get_regional_autocorr(trials[[i]])
    eigen.global <- get_autocorr_eigenvalues(autocorr.reg) # single 5-vector
    # 5 features each:
    eigen.rolling3 <- get_rolling_autocorr_eigens(autocorr.reg, window=3)
    eigen.rolling7 <- get_rolling_autocorr_eigens(autocorr.reg, window=7)
    eigen.rolling10 <- get_rolling_autocorr_eigens(autocorr.reg, window=10)
     
    t <- tibble(
      feedback = rep(trial[[1]]$feedback[[1]], 40),
      l_contrast = rep(trial[[1]]$contrast_left[[1]], 40),
      r_contrast = rep(trial[[1]]$contrast_right[[1]], 40),
      #mean_overall = means[[2]], 
      #median_overall = medians[[2]],
      #sd_overall = sd[[2]],
      #skew_overall = skew[[2]], 
      #kurt_overall = kurt[[2]],
      #der1_overall = first_deriv[[2]],
      #der2_overall = second_deriv[[2]],
      #means_roll3 = means.roll3[[2]], 
      #means_roll7 = means.roll7[[2]],
      #means_roll10 = means.roll10[[2]],
      #medians_roll3 = medians.roll3[[2]],
      #medians_roll7 = medians.roll7[[2]],
      #medians_roll10 = medians.roll10[[2]],
      #sd_roll3 = sd.roll3[[2]], 
      #sd_roll7 = sd.roll7[[2]],
      #sd_roll10 = sd.roll10[[2]],
      #skew_roll3 = skew.roll3[[2]], 
      #skew_roll7 = skew.roll7[[2]],
      #skew_roll10 = skew.roll10[[2]],
      #kurt_roll3 = kurt.roll3[[2]], 
      #kurt_roll7 = kurt.roll7[[2]],
      #kurt_roll10 = kurt.roll10[[2]],
      der1_roll3 = der1.roll3[[2]],
      der1_roll7 = der1.roll7[[2]],
      der1_roll10 = der1.roll10[[2]],
      der2_roll3 = der2.roll3[[2]],
      der2_roll7 = der2.roll7[[2]],
      der2_roll10 = der2.roll10[[2]]
    )
    
    eigen3 <- eigen.rolling3 %>% select(-1)
    t <- bind_cols(t, eigen3)
    eigen7 <- eigen.rolling7 %>% select(-1)
    t <- bind_cols(t, eigen7)
    eigen10 <- eigen.rolling10 %>% select(-1)
    t <- bind_cols(t, eigen10)
    
    # Positional encoding
    pos <- get_pos_indices(d=40) %>% select(-1) 
    t <- bind_cols(t, pos)
    
    return(t)
  })
  
  return(transformer.ready) 
}

process_validation_data <- function(one_or_two=1) {
  trials <- list()
  trials <- load_validation_sessions()  
  if (one_or_two == 1) {
    trials <- trials[[1]]
  } else {
    trials <- trials[[2]]
  }
  
  trial.data <- mine_data(trials)
  trial.matrices <- lapply(trial.data, function(x) {
    x %>% mutate(across(everything(), ~ as.numeric(.)))
    as.matrix(x)
  })
  trial.matrices <- Filter(function(x) nrow(x) >= 40, trial.matrices)
  trial.dims <- dim(trial.matrices[[1]])
  trial.array <- array(unlist(trial.matrices), 
                       dim=c(trial.dims[1],
                             trial.dims[2],
                             length(trial.matrices)))
  trial.array <- aperm(trial.array, c(3,1,2))
  
  print(dim(trial.array))
  np <- import("numpy")
  np$save(paste0("../Data/validation_der",one_or_two,".npy"), trial.array)
}

# Main
main <- function() {
  set.seed(123)
  
  trials <- load_unified_sessions()
  print_yn_counts(trials)
  trials <- equalize_yn_trials(trials)
  print_yn_counts(trials)
  
  train_valid <- gen_train_val_sets(trials)
  train <- train_valid[[1]]
  valid <- train_valid[[2]]
  
  train.data <- mine_data(train)
  valid.data <- mine_data(valid)
  
  # Convert from chr to num (as.matrix implicit conversion possible)
  train.matrices <- lapply(train.data, function(x) {
    x %>% mutate(across(everything(), ~ as.numeric(.)))
    as.matrix(x)
  })
  valid.matrices <- lapply(valid.data, function(x) {
    x %>% mutate(across(everything(), ~ as.numeric(.)))
    as.matrix(x)
  })
 
  # Remove any trials with nrow() < 40
  train.matrices <- Filter(function(x) nrow(x) >= 40, train.matrices)
  valid.matrices <- Filter(function(x) nrow(x) >= 40, valid.matrices)
   
  # Validate NAs
  lapply(seq_along(train.matrices), function(x) {
    na.idx <- which(rowSums(is.na(train.data[[x]])) > 0)
    if (length(na.idx) > 0) {
      print(x)
      print(train.matrices[[x]][na.idx, , drop=FALSE])
    }
  })
  lapply(seq_along(valid.matrices), function(x) {
    na.idx <- which(rowSums(is.na(valid.data[[x]])) > 0)
    if (length(na.idx) > 0) {
      print(x)
      print(valid.matrices[[x]][na.idx, , drop=FALSE])
    }
  })
  print(any(is.na(train.data)))
  print(any(is.na(valid.data)))
  
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
  np$save("../Data/full_updated_train.npy", train.array)
  np$save("../Data/full_updated_valid.npy", valid.array)
}

# Debug function; completely variable function
debug <- function() {
  set.seed(123)
  trials <- load_unified_sessions()
  trials <- equalize_yn_trials(trials)
  train <- gen_train_val_sets(trials, 1)
  
  train.data <- mine_data(train[2748])
  print(train.data[[1]], width=Inf, n=Inf)
}

#debug()
for (i in 1:2) {
  process_validation_data(i)
}
#main()

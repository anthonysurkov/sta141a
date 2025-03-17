library(ggplot2)

# Load sessions
session5 <- readRDS("../Data/session5.rds")
session5.11 <- session5$spks[[11]]

# Calculate autocorrelations for trial 11 of session 5 as an experiment
acfs <- sapply(1:nrow(session5.11), function(n) {
        acf(session5.11[n,], lag.max=100, plot=FALSE)$acf
})
avg.acf <- rowMeans(acfs, na.rm=TRUE)
lags <- acf(session5.11[1,], lag.max=100, plot=FALSE)$lag
  
# Compute autocorrelation
acf.df <- data.frame(lag = lags, acf = avg.acf)
acf.df <- na.omit(acf.df)

# Plot autocorrelation
acf.plot <- ggplot(acf.df, aes(x = lag, y = acf)) +
            geom_line() +
            geom_point() +
            labs(
              title = "Autocorrelation for Session 5, Trial 11",
              x = "Lag (time steps)", 
              y = "Autocorrelation") +
            theme_minimal()
print(acf.plot)

# No clear temporal relationship between random neurons. OK, somewhat expected
# Let's try brain regions.

# Find all unique brain regions in session 5:
session5.regions <- unique(session5$brain_area)

session5.regional <- lapply(session5.regions, function(region) {
  neurons <- which(session5$brain_area == region)
  rowMeans(session5$spks[[11]][neurons, , drop=FALSE])
})

session5.region_matrix <- do.call(rbind, session5.regional)
rownames(session5.region_matrix) <- session5.regions

session5.11.r_acfs <- lapply(session5.regions, function(region) {
  my.acf <- acf(session5.region_matrix[region, ], lag.max=100, plot=FALSE)
  data.frame(lag = my.acf$lag, 
             acf = as.vector(my.acf$acf), 
             region=region)
})

# Find all autocorrelations by brain region in session 5, trial 11:
acf.df <- do.call(rbind, session5.11.r_acfs)
acf.df <- acf.df %>% # smooth it
          group_by(region) %>%
          mutate(acf.smooth = zoo::rollmean(acf, k=5, fill=NA, align="center", partial=TRUE))

acf.plot <- ggplot(acf.df, aes(x=lag, y=acf.smooth, color=region)) +
            geom_line(na.rm=TRUE) +
            geom_point(na.rm=TRUE) +
            labs(
              title="Autocorrelation by Brain Region",
              x="Lag (time steps)",
              y="Autocorrelation"
            ) +
            theme_minimal()
print(acf.plot)

# Quantify memory of brain regions
session5.memory <- acf.df %>%
  group_by(region) %>%
  summarize(acf.mean = mean(abs(acf)))

print(session5.memory)


# Q1: differentiability between feedback={-1,1} and L/R permutations based 
#     on this?

# Select relevant data from session 5
session5.df <- lapply(seq_along(session5$spks), function(i) {
  as.data.frame(session5$spks[[i]]) %>%
    mutate(
      NeuronID = row_number(),
      contrast.left = session5$contrast_left[i],
      contrast.right = session5$contrast_right[i],
      feedback = session5$feedback_type[i],
      region = session5$brain_area
    )
}) %>% bind_rows()

# Find unique brain areas, regions
session5$brain_area %>% unique() %>% print()
session5.df$region %>% unique() %>% print()

# Identify all permutations of unique contrast levels
session5.cl <- session5$contrast_left %>% unique()
session5.cr <- session5$contrast_right %>% unique()
session5.contrasts <- expand.grid(
  contrast.left = session5.cl,
  contrast.right = session5.cr
) %>% arrange(desc(contrast.right))

# Generate L/R permutation subsets
session5.lrs <- list()
for (i in 1:nrow(session5.contrasts)) {
  l <- session5.contrasts$contrast.left[[i]]
  r <- session5.contrasts$contrast.right[[i]]
  
  session5.filter <- session5.df %>%
                     filter(contrast.left == l & contrast.right == r)
  
  session5.lrs[[paste0("L", l, "_R", r)]] <- session5.filter
}

session5.df.y <- session5.df %>% 
                 filter(feedback == 1) # generate feedback=y subset
session5.df.n <- session5.df %>% 
                 filter(feedback == -1) # generate feedback=n subset


# Grab brain regions by feedback = -1/1
session5.y.regions <- unique(session5.df.y$region)
session5.n.regions <- unique(session5.df.n$region)

session5.y.regional <- lapply(session5.y.regions, function(reg) {
  neurons <- which(session5.df.y$region == reg)
})
session5.n.regional <- lapply(session5.n.regions, function(reg) {
  neurons <- which(session5.df.n$region == reg)
})


# Function to compute an average timeseries by region
compute.region_timeseries <- function(df, region.name) {
  region.df <- df %>% filter(region == region.name)   
  
  region.df.times <- region.df %>%
                     select(-NeuronID, -region, -feedback, -contrast.left, -contrast.right)
  
  if (nrow(region.df.times) == 0) {
    return(NULL)
  }
  return( colMeans(region.df.times, na.rm=TRUE) )
}


# Function to compute autocorrelations from average region timeseries
compute.acf <- function(region.vector, max_lag=100) {
  if (is.null(region.vector)) {
    return(NULL)
  }
  my.acf <- acf(region.vector, lag.max=max_lag, plot=FALSE)
  data.frame(
    lag = my.acf$lag,
    acf = my.acf$acf
  )
}


# Compute autocorrelates for feedback=1 trials
session5.y.acfs <- lapply(unique(session5.df.y$region), function(reg) {
  region.vector <- compute.region_timeseries(session5.df.y, reg)
  acf.df <- compute.acf(region.vector)
  
  if (!is.null(acf.df)) {
    acf.df$region <- reg
    acf.df$feedback <- 'y'
  }
  return(acf.df)
})
session5.y.acfs <- session5.y.acfs[!sapply(session5.y.acfs, is.null)]
session5.y.acfs.df <- bind_rows(session5.y.acfs)


# Compute autocorrelates for feedback=-1 trials
session5.n.acfs <- lapply(unique(session5.df.n$region), function(reg) {
  region.vector <- compute.region_timeseries(session5.df.n, reg)
  acf.df <- compute.acf(region.vector)
  
  if (!is.null(acf.df)) {
    acf.df$region <- reg
    acf.df$feedback <- 'n'
  }
  return(acf.df)
})
session5.n.acfs <- session5.n.acfs[!sapply(session5.n.acfs, is.null)]
session5.n.acfs.df <- bind_rows(session5.n.acfs)


# Combine all and plot to see any regional differences
session5.yn.acfs.df <- bind_rows(session5.y.acfs.df, session5.n.acfs.df)

session5.yn.plot <- ggplot(session5.yn.acfs.df, aes(x=lag, y=acf, color=feedback)) +
                    geom_line(na.rm=TRUE) +
                    facet_wrap(~ region, scales="free_y") +
                    labs(
                      title="Region-wise Autocorrelation by Feedback",
                      x="Lag (time steps)",
                      y="Autocorrelation"
                    ) +
                    theme_minimal()
print(session5.yn.plot)


# Q2: correlation between brain regions as a feature?
# Exploratory: plot one trial's region-wise autocorrelation correlations
acf.wide <- acf.df %>%
            select(lag, region, acf.smooth) %>%
            spread(key=region, value=acf.smooth)
acf.cor.matrix <- cor(acf.wide[,-1], use="pairwise.complete.obs")

acf.cor.long <- as.data.frame(as.table(acf.cor.matrix)) %>%
                arrange(desc(abs(Freq)))
acf.cor.plot <- ggplot(acf.cor.long, aes(Var1, Var2, fill=Freq)) +
                geom_tile() +
                scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0) +
                theme_minimal() +
                labs(
                  title="Region-wise Autocorrelation Correlations (Session 5.11)",
                  x="Region",
                  y="Region"
                )
print(acf.cor.plot)


# Identify feedback=1, feedback=-1 matrices as for Q1 and plot accordingly
y.acf.wide <- session5.y.acfs.df %>%
              select(lag, region, acf) %>%
              spread(key=region, value=acf)
y.acf.cor.matrix <- cor(y.acf.wide[,-1], use="pairwise.complete.obs")

y.acf.cor.long <- as.data.frame(as.table(y.acf.cor.matrix)) %>%
                  arrange(desc(abs(Freq)))
y.acf.cor.plot <- ggplot(y.acf.cor.long, aes(Var1, Var2, fill=Freq)) +
                  geom_tile() +
                  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0) +
                  theme_minimal() +
                  labs(
                    title="Region-wise Autocorrelation Correlations, Feedback=Y",
                    x="Region",
                    y="Region"
                  )
print(y.acf.cor.plot)


n.acf.wide <- session5.n.acfs.df %>%
              select(lag, region, acf) %>%
              spread(key=region, value=acf)
n.acf.cor.matrix <- cor(n.acf.wide[,-1], use="pairwise.complete.obs")

n.acf.cor.long <- as.data.frame(as.table(n.acf.cor.matrix)) %>%
                  arrange(desc(abs(Freq)))
n.acf.cor.plot <- ggplot(n.acf.cor.long, aes(Var1, Var2, fill=Freq)) +
                  geom_tile() +
                  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0) +
                  theme_minimal() +
                  labs(
                    title="Region-wise Autocorrelation Correlations, Feedback=N",
                    x="Region",
                    y="Region"
                  )
print(n.acf.cor.plot)

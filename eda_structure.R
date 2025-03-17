library(tidyverse)
library(dplyr)
library(ggplot2)
library(purrr)


# Objective:
# describe the data structures across sessions 
# (e.g., number of neurons, number of trials, stimuli conditions, 
# feedback types) 

# 1. Distributions of number of neurons and number of trials.
make.neuron_trial_charts <- function(neuron.data, trial.data, 
                                     neural.title, trial.title) {
  neuron.chart <- ggplot(
    aes(x=neurons, fill=as.factor(mouse.name)), data=neuron.data) +
    geom_histogram(bins=30, position="stack") +
    labs(
      title=neural.title,
      x="Number (neurons)",
      y="Frequency",
      fill="Mouse ID"
    ) +
    theme_minimal()
  print(neuron.chart)
  
  trial.chart <- ggplot(
    aes(x=trials, fill=as.factor(mouse.name)), data=trial.data) +
    geom_histogram(bins=30, position="stack") +
    labs(
      title=trial.title,
      x="Number (trials)",
      y="Frequency",
      fill="Mouse ID"
    ) +
    theme_minimal()
  print(trial.chart)
}

neuron.data <- data.frame()
trial.data <- data.frame()
for (i in 1:18) {
  session <- readRDS(paste("../Data/session", i, ".rds", sep=""))
  
  neuron_count <- sum(sapply(session$spks, length))
  neuron_avg <- neuron_count / length(session$spks)
    neuron_count <- sum(sapply(session$spks, length))
  neuron_avg <- neuron_count / length(session$spks)
  
  neuron.data <- rbind(neuron.data, data.frame(
    neurons = neuron_avg,
    mouse.name = session$mouse_name
  ))
  trial.data <- rbind(trial.data, data.frame(
    trials = length(session$feedback_type),
    mouse.name = session$mouse_name
  ))
}

make.neuron_trial_charts(neuron.data, trial.data,
                         "Neuron Count by Session",
                         "Trial Count by Session")


# 2. Frequencies of permutations of L/R combinations, along with their 
#    success rates
contrast.data <- data.frame()
for (i in 1:18) {
  session <- readRDS(paste("../Data/session", i, ".rds", sep=""))
  
  session.contrasts <- data.frame(
    contrast.left = unlist(session$contrast_left),
    contrast.right = unlist(session$contrast_right),
    feedback.type = unlist(session$feedback_type)
  )
  contrast.data <- rbind(contrast.data, session.contrasts)
}

contrast.freq <- contrast.data %>%
  count(contrast.left, contrast.right) %>%
  arrange(desc(n))

contrast.plot <- ggplot(data=contrast.freq, aes(x=interaction(contrast.left, 
                                                              contrast.right, 
                                                              sep="; "), y=n)) +
                 geom_bar(stat="identity", fill="steelblue") +
                 labs(
                   title="Frequency of Contrast Left-Right Pairs",
                   x="Contrast (Left; Right)",
                   y="Frequency"
                 ) +
                 theme_minimal() +
                 theme(axis.text.x = element_text(angle=45, hjust=1))
print(contrast.plot)

contrast.feedback <- contrast.data %>%
  group_by(contrast.left, contrast.right) %>%
  summarize(
    success_rate = sum(feedback.type == 1) / length(feedback.type),
    .groups="drop"
  ) %>%
  mutate(contrast.pair=interaction(contrast.left, contrast.right, sep="; ")) %>% 
  arrange(success_rate) %>%
  mutate(contrast.pair = factor(contrast.pair, levels=contrast.pair))

contrast.feedback.plot <- ggplot(data=contrast.feedback, aes(x=contrast.pair, 
                                                             y=success_rate)) +
                          geom_bar(stat="identity", fill="steelblue") +
                          labs(
                            title="Success rate by Contrast Left-Right Pair",
                            x="Contrast (Left; Right)",
                            y="Success rate (# correct / total)"
                          ) +
                          theme_minimal() +
                          theme(axis.text.x = element_text(angle=45, hjust = 1))
print(contrast.feedback.plot)


# (2) figure out which brain regions are conserved across regions, if any
sessions <- list()
for (i in 1:18) {
  sessions[[i]] <- readRDS(paste("../Data/session", i, ".rds", sep='')) 
  print(sessions[[i]]$mouse_name)
}

# find all unique regions
all_regions <- lapply(sessions, function(s) {
  s$brain_area %>% unique()
}) %>%
  unlist() %>%
  unique()

# print conserved regions
conserved_regions <- Reduce(intersect, lapply(sessions, function(s) s$brain_area))
#print(conserved_regions) # none

# print brain regions by session
lapply(sessions, function(s) s$brain_area %>% unique()) #%>% print())

# print minimum # brain regions (important to know for PCA analysis)
lapply(sessions, function(s) s$brain_area %>% unique() %>% length()) %>% 
  unlist() %>% min() #%>% print()

session1 <- sessions[[1]]
print(session1$brain_area[[1]])

# (3) Miscellaneous basic information
# Time range
session5 <- sessions[[i]]
session5.t <- session5$time[[11]]
lapply(seq_along(this.session$time), function(i) {
  session5.t <- session5$time[[i]]
  print(max(session5.t) - min(session5.t))
})

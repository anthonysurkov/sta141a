library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(tsne)

# Is data separable by PCA based on inputs?
# Short answer: no

session5 <- readRDS("../Data/session5.rds")

session5.spks_df <- lapply(seq_along(session5$spks), function(i) {
  as.data.frame(session5$spks[[i]]) %>%
    mutate(NeuronID = row_number(), 
           Trial = i,
           contrast.left = session5$contrast_left[i],
           contrast.right = session5$contrast_right[i])
})

# Inputs: mouse (accounted for by selecting only session5)
#         L/R contrast; all permutations

session5.cl <- session5$contrast_left %>% unique()
session5.cr <- session5$contrast_right %>% unique()
session.contrasts <- expand.grid(
  contrast.left = session5.cl,
  contrast.right = session5.cr
) %>% arrange(desc(contrast.right))
# session.contrasts now holds all 16 permutations of L/R inputs

session5.lrs = list()
for (i in 1:16) {
  l <- session.contrasts$contrast.left[[i]]
  r <- session.contrasts$contrast.right[[i]]
  
  session5.filter <- session5.spks_df %>%
                     bind_rows() %>%
                     filter(contrast.left == l & contrast.right == r)
  
  session5.lrs[[paste0("L", l, "_R", r)]] <- session5.filter
}
# session5.lrs now holds all L/R permutation-associated data

session5.pcas = list()
for (i in 1:16) {
  session5.combined <- session5.lrs[[i]] %>%
                       select(-NeuronID, -Trial, -contrast.left, -contrast.right) %>%
                       as.matrix()
  
  pca <- prcomp(session5.combined)
  pca.df <- data.frame(PC1=pca$x[,1], PC2=pca$x[,2]) 

  l <- unique(session5.lrs[[i]]$contrast.left)
  r <- unique(session5.lrs[[i]]$contrast.right)
  lr <- paste0("L", l, "_R", r)
  pca.title <- paste0("PCA for L=", l, ", R=", r)
  
  pca.plot <- ggplot(pca.df, aes(x=PC1, y=PC2)) +
              geom_point(alpha=0.7) +
              labs(
                title=pca.title,
                x="PC1",
                y="PC2"
              ) +
              theme_minimal()
  
  session5.pcas[[lr]] <- pca.plot
}

#session5.plots <- split(session5.pcas, ceiling(seq_along(session5.pcas) / 4))
#for (i in 1:4) {
#  do.call(grid.arrange, c(session5.plots[[i]], ncol=2))
#}


# (3) Is data separable by a nonlinear method, given PCA's failure?
# Try: tsne

session5.tsne <- tsne(session5.lrs[["L1_R0"]], initial_dims=2)
session5.tsne <- data.frame(session5.tsne)

print(summary(session5.tsne))

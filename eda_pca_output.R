# EDA, interest 1: Is data separable by PCA based on outputs?

# First: we need a way to visualize neural response.
# Easy way: proportion of neurons active. this may ablate important information 
# though
# Potentially better way: PCA between sessions of interest. Dimensionality 
# reduction from session x neuron x time to principal components


# A case study (for learning)
session5 <- readRDS("../Data/session5.rds")
trial1 <- as.data.frame(session5$spks[[11]]) %>%
  mutate(NeuronID = row_number())
trial2 <- as.data.frame(session5$spks[[12]]) %>%
  mutate(NeuronID = row_number())
combined <- full_join(trial1, trial2, by="NeuronID") %>%
  select(-NeuronID) %>%
  as.matrix

pca <- prcomp(combined, center=TRUE, scale.=TRUE)
print(summary(pca))

pca.df <- data.frame(PC1=pca$x[,1], PC2=pca$x[,2])
pca.plot <- ggplot(pca.df, aes(x=PC1, y=PC2)) +
  geom_point(alpha=0.7) +
  theme_minimal() +
  labs(
    title="PCA of Neural Activity Across Trials 11 & 12 of Session 5",
    x="PC1",
    y="PC2"
  )
#print(pca.plot)


# ===== TRY EXTENDING TO AN ENTIRE SESSION ===== #

session5.df <- lapply(seq_along(session5$spks), function(i) {
  as.data.frame(session5$spks[[i]]) %>%
    mutate(NeuronID=row_number(), Trial=i)
})
session5.ft <- session5$feedback_type
session5.ft.y <- which(session5.ft == 1)
session5.ft.n <- which(session5.ft == -1)

session5.df.y <- session5.df[session5.ft.y]
session5.df.n <- session5.df[session5.ft.n]

session5.combined.y <- Reduce(function(x,y) full_join(x, y, by="NeuronID"), session5.df.y) %>%
  select(-NeuronID) %>%
  as.matrix()

session5.combined.n <- Reduce(function(x,y) full_join(x, y, by="NeuronID"), session5.df.n) %>%
  select(-NeuronID) %>%
  as.matrix()

pca.y <- prcomp(session5.combined.y)
pca.y.df <- data.frame(PC1=pca.y$x[,1], PC2=pca.y$x[,2], Label="Yes")
var.y <- (pca.y$sdev^2) / sum(pca.y$sdev^2)
print(var.y[1:10])

pca.n <- prcomp(session5.combined.n)
pca.n.df <- data.frame(PC1=pca.n$x[,1], PC2=pca.n$x[,2], Label="No")
var.n <- (pca.n$sdev^2) / sum(pca.y$sdev^2)
print(var.n[1:10])

pca.yn.df <- rbind(pca.y.df, pca.n.df)

pca.y.plot <- ggplot(pca.y.df, aes(x=PC1, y=PC2)) +
  geom_point(alpha=0.7) +
  theme_minimal() +
  labs(
    title="PCA of Neural Activity Across Successful Mouse Decisions",
    x="PC1",
    y="PC2"
  )
print(pca.y.plot)

pca.n.plot <- ggplot(pca.n.df, aes(x=PC1, y=PC2)) +
  geom_point(alpha=0.7) +
  theme_minimal() +
  labs(
    title="PCA of Neural Activity Across Unsuccessful Mouse Decisions",
    x="PC1",
    y="PC2"
  )
print(pca.n.plot)

pca.yn.plot <- ggplot(pca.yn.df, aes(x=PC1, y=PC2, color=Label)) +
  geom_point(alpha=0.7) +
  theme_minimal() +
  labs(
    title="Neural Activity PCA Across Yes/No Decisions (Session 5)",
    x="PC1",
    y="PC2"
  )
print(pca.yn.plot)
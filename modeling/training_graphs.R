library(tidyverse)
library(ggplot2)
library(patchwork)


make_graphs <- function(name) {
  data <- read_csv(paste0("Data/data_",name,".csv"))
  print(head(data))


  acc.long <- data %>%
    select(epoch, train_acc, valid_acc) %>%
    pivot_longer(cols = c(train_acc, valid_acc), 
                 names_to="Metric",
                 values_to="accuracy")

  loss.long <- data %>%
    select(epoch, train_loss, valid_loss) %>%
    pivot_longer(cols = c(train_loss, valid_loss),
                 names_to="Metric",
                 values_to="accuracy")

  acc.graph <- ggplot(aes(x=epoch, y=accuracy, color=Metric), data=acc.long) +
    geom_line(linewidth=1) +
    geom_point(size=2) +
    labs(
      title=paste0("Training and Validation for Updated Full Model (d=64, lr=1e-4)"),
      x="Model Epoch",
      y="Accuracy"
    ) +
    theme_minimal()

  loss.graph <- ggplot(aes(x=epoch, y=accuracy, color=Metric), data=loss.long) +
    geom_line(linewidth=1) +
    geom_point(size=2) +
    labs(
      x="Model Epoch",
      y="Loss"
    ) +
    theme_minimal()

  print(acc.graph + loss.graph)
}

make_graphs("ufull_d32_l1e-5")

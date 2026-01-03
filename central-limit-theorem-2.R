library(tidyverse)
library(gridExtra)

fnc <- function(n){
  vec <- numeric(n)
  
  for(i in 1:n){
    vec[i] <- sum(sample(1:6, 8, replace = TRUE))
  }
  return(as.data.frame(vec))
}

plots <- list()

for(i in c(50, 100, 500, 10000)) {
  data <- fnc(i)
  plots[[as.character(i)]] <- data %>% 
    ggplot(aes(x = vec)) +
    geom_bar(fill = "lightblue", size = .5) +
    stat_density(geom = "line", aes(y = ..density.. * n), 
                 color = "orange", linewidth = .8) +
    scale_x_continuous(breaks = seq(min(data$vec), max(data$vec), by = 3)) +
    theme_minimal() +
    labs(title = paste0("8d6 Roll Frequency (", length(data$vec), " Rolls)"),
         x = "Roll",
         y = "Count") +
    theme(plot.title = element_text(hjust = .5))
}

grid.arrange(grobs = plots)



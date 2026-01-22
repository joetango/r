library(tidyverse)
library(gridExtra)

fnc <- function(n){
  vec <- numeric(n)
  
  for(i in 1:n){
    vec[i] <- sum(sample(1:6, 6, replace = TRUE))
  }
  return(as.data.frame(vec))
}


data <- fnc(1000)


data %>% 
  ggplot(aes(x = vec)) +
  geom_bar(fill = "lightblue", size = .5) +
  stat_density(geom = "line", aes(y = ..density.. * n), 
               color = "orange", linewidth = .8) +
  scale_x_continuous(breaks = seq(min(data$vec), max(data$vec), by = 1)) +
  theme_minimal() +
  labs(title = paste("6d6 Roll Frequency With", length(data$vec), "Rolls")) +
  theme(plot.title = element_text(hjust = .5))




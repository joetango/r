library(tidyverse)
library(gridExtra)

fnc <- function(n){
  vec <- numeric(n)
  
  for(i in 1:n){
    vec[i] <- sum(sample(1:6, 6, replace = TRUE))
  }
  return(as.data.frame(vec))
}

roll50 <- fnc(50)
roll100 <- fnc(100)
roll1000 <- fnc(1000)
roll10000 <- fnc(10000)


p1 <- roll50 %>% 
  ggplot(aes(x = vec)) +
  geom_bar(fill = "lightblue", size = .5) +
  stat_density(geom = "line", aes(y = ..density.. * n), 
               color = "orange", linewidth = .8) +
  scale_x_continuous(breaks = seq(min(roll50$vec), max(roll50$vec), by = 1)) +
  theme_minimal() +
  labs(title = paste("6d6 Roll Frequency With", length(roll50$vec), "Rolls")) +
  theme(plot.title = element_text(hjust = .5))


p2 <- roll100 %>% 
  ggplot(aes(x = vec)) +
  geom_bar(fill = "lightblue", size = .5) +
  stat_density(geom = "line", aes(y = ..density.. * n), 
               color = "orange", linewidth = .8) +
  scale_x_continuous(breaks = seq(min(roll100$vec), max(roll100$vec), by = 1)) +
  theme_minimal() +
  labs(title = paste("6d6 Roll Frequency With", length(roll100$vec), "Rolls")) +
  theme(plot.title = element_text(hjust = .5))

p3 <- roll1000 %>% 
  ggplot(aes(x = vec)) +
  geom_bar(fill = "lightblue", size = .5) +
  stat_density(geom = "line", aes(y = ..density.. * n), 
               color = "orange", linewidth = .8) +
  scale_x_continuous(breaks = seq(min(roll1000$vec), max(roll1000$vec), by = 1)) +
  theme_minimal() +
  labs(title = paste("6d6 Roll Frequency With", length(roll1000$vec), "Rolls")) +
  theme(plot.title = element_text(hjust = .5))

p4 <- roll10000 %>% 
  ggplot(aes(x = vec)) +
  geom_bar(fill = "lightblue", size = .5) +
  stat_density(geom = "line", aes(y = ..density.. * n), 
               color = "orange", linewidth = .8) +
  scale_x_continuous(breaks = seq(min(roll10000$vec), max(roll10000$vec), by = 1)) +
  theme_minimal() +
  labs(title = paste("6d6 Roll Frequency With", length(roll10000$vec), "Rolls")) +
  theme(plot.title = element_text(hjust = .5))

grid.arrange(p1, p2, p3, p4)

require(tidyverse)


test_num <- c(1, 2, 3, 4, 5)
time_left <- c(.291, .336, .347, .304, .298)
time_right <- c(.317, .29, .353, .286, .317)

df <- data.frame(TestNumber = test_num, LefthandTime = time_left,
                 RighthandTime = time_right)

df %>%
  ggplot() +
  geom_point(aes(x = TestNumber, y = LefthandTime,
                 color = "Left Hand Time"), size = 3) +
  geom_text(aes(x = TestNumber, y = LefthandTime,
                label = "L"), size = 5, hjust = -.5) +
  geom_point(aes(x = TestNumber, y = RighthandTime,
                 color = "Right Hand Time"), size = 3) +
  geom_text(aes(x = TestNumber, y = RighthandTime,
                label = "R", hjust = -.5), size = 5) +
  labs(title = "Lefthand vs. Righthand Reaction Times",
       x = "Test Number", y = "Time (seconds)", colour = "Key") +
  xlim(1,5.5) +
  ylim(.275,.375)


library(NHANES)
library(tidyverse)
data(NHANES)

ggplot(data = subset(NHANES, !is.na(Diabetes)),
       aes(x = Poverty, y = BMI, color = Diabetes)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = "lm", size = 2) +
  facet_grid(Gender ~ SurveyYr) +
  scale_color_manual(values = c("No" = "purple4",
                                "Yes" = "lightgreen")) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "gray90")) 

NHANES1 <- NHANES[!is.na(NHANES$Diabetes) & !is.na(NHANES$Education), ]

NHANES1 %>% 
  ggplot(aes(x = TotChol, y = Education, color = Diabetes)) +
  geom_boxplot(fill = "gray", alpha = 0.3, lwd = .8) +
  facet_grid( ~ Gender) +
  scale_color_manual(values = c("Yes" = "purple4",
                                "No" = "lightgreen")) +
  theme_minimal() +
  labs(x = "Total Cholesterol", y = "Education") 
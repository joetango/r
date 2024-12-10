require(tidyverse)

trashwheel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-03-05/trashwheel.csv')

trashwheel %>% 
  mutate(Date = as_date(Date, format=c("%m/%d/%Y", "%m/%d/%y"))) %>% 
  group_by(Name, Date) %>% 
  summarize(tot_weight = sum(Weight)) %>% 
  arrange(Date)



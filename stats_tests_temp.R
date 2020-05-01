
library(tidyverse)
library(nycflights13)
install.packages("tidystats")
library(tidystats)

install.packages("rstatix")
library(rstatix)

flights %>% freq_table(dest)

flights %>% identify_outliers(dep_delay)

flights %>% get_summary_stats(dep_delay)

flights %>% sample_n(1000) %>% shapiro_test(dep_delay)

flights %>% t_test(dep_delay~origin, detailed=T)

#flights %>% chisq_test(origin)

flights %>% filter(origin!="LGA") %>% 
  t.test(dep_delay~origin, data=.) %>% 
  tidy()

flights %>% filter(origin!="LGA") %>% 
  pull(dep_delay) %>%
  t.test(mu=13) %>% 
  tidy()

flights %>% sample_n(1000) %>% 
  pull(dep_delay) %>% 
  shapiro.test() %>% 
  tidy()

flights %>% 
  cor.test(formula= ~ dep_delay + dep_time, data=.) %>%
  tidy()

flights %>% 
  cor.test(formula= ~ dep_delay + dep_time, data=.) %>%
  tidy()

flights %>% group_by(origin, dest) %>%
  tally() %>%
  pivot_wider(names_from="origin", values_from="n", values_fill=list(n=0)) %>%
  select(-dest) %>%
  chisq.test() %>%
  tidy()

#https://infer.netlify.app/

#glm

library(zeligverse)
#zelig


#Interactions in models, I()

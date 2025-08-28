library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)


# Grouping ----------------------------------------------------------------

## combining group_by() with summarize()
df_m_sd <- iris_sub %>% 
  group_by(Species) %>% 
  summarize(mean_sl = mean(Sepal.Length), 
            sd_sl = sd(Sepal.Length))

## combining group_by() with mutate()
df_eps <- iris_sub %>% 
  group_by(Species) %>% 
  mutate(mean_sl = mean(Sepal.Length)) %>% 
  ungroup() %>% 
  mutate(eps = abs(Sepal.Length - mean_sl))

# Reshaping ---------------------------------------------------------------






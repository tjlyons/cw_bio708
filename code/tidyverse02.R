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

## wide format
iris_w <- iris_sub %>% 
  mutate(id = rep(1:3, 3)) %>%  # add an ID column
  select(id, Sepal.Length, Species) %>% 
  pivot_wider(id_cols = "id", # unique row ID based on
              values_from = "Sepal.Length", # values in each cell from
              names_from = "Species") # new column names from

## long format
iris_l <- iris_w %>% 
  pivot_longer(cols = c("setosa", 
                        "versicolor",
                        "virginica"),
               names_to = "Species",
               values_to = "Sepal.Length"
               )

# joining -----------------------------------------------------------------

df1 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3))

df2 <- tibble(Species = c("A", "B", "C"),
              y = c(4, 5, 6))

df12 <- left_join(x = df1,
                  y = df2,
                  by = "Species")

## what happens if df2 does not contain species b?
df2_minusB <- tibble(Species = c("A", "C"),
                     y = c(4,6))

left_join(x = df1,
          y = df2_minusB,
          by = "Species")

left_join(x = df2_minusB,
          y = df1,
          by = "Species")


# additional resource -----------------------------------------------------


## library(swirl)
## install_course_github("sysilviakim", "swirl-tidy")

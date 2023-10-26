#' ---
#' title: "Report week3"
#' output: html_document
#' date: "2023-08-29"
#' author: Akira Terui
#' ---

library(tidyverse)

# basic plot
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)) +
  geom_point()

# change color by "Species" column
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width,
             color = Species)) +
  geom_point()

# sample data
df0 <- tibble(x = rep(1:50, 3),
              y = x * 2)

# basic plot
df0 %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() + 
  geom_point()

print(df0)

# basic plot; bins = 30 by default
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram()

# change bin width
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5)

# change bin number
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(bins = 50)

# basic plot
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length)) +
  geom_boxplot()

# change fill by "Species"
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot()

# change fill by "Species", but consistent color
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot(color = "darkgrey")

# density plot
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_density(fill = "salmon",
               color = "salmon",
               alpha = 0.5)

# facet: facet_wrap
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)) + 
  geom_point() +
  facet_wrap(facets = ~ Species,
             nrow = 2,
             ncol = 2)

# facet: facet_grid
iris %>% 
  mutate(site = sample(letters[1:2], nrow(.), replace = TRUE)) %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)) + 
  geom_point() +
  facet_grid(rows = vars(Species),
             cols = vars(site)) +
  theme_bw() +
  theme(panel.grid = element_blank())


# extra exercise ----------------------------------------------------------

# Q1: create 10 by 10 matrix; elements may be random numbers
## m1
m_ran <- matrix(rnorm(n = 100),
                nrow = 10,
                ncol = 10)

## m2; this works too, but redundant
# rbind(rnorm(10),
#       rnorm(10),
#       rnorm(10),
#       rnorm(10),
#       rnorm(10),
#       rnorm(10),
#       rnorm(10),
#       rnorm(10),
#       rnorm(10),
#       rnorm(10))

# Q2: calculate means for each row

## easiest, but ugly
# mean(m_ran[1, ])
# mean(m_ran[2, ])
# mean(m_ran[3, ])
# mean(m_ran[4, ])
# mean(m_ran[5, ])
# mean(m_ran[6, ])
# mean(m_ran[7, ])
# mean(m_ran[8, ])
# mean(m_ran[9, ])
# mean(m_ran[10, ])

## better
v_row_mu <- rowMeans(m_ran)

## possible alternative
v_row_mu_for <- NULL
for (i in 1:10) {
  # task gores here
  v_row_mu_for[i] <- mean(m_ran[i, ])  
}

# Q3: create a tibble with three columns of numbers, and one column of letters (a-z)
# # rows is 100
# for letters, use sample(letters, 100, replace = TRUE)

library(tidyverse)

set.seed(123)
df0 <- tibble(x1 = rnorm(100),
              x2 = rpois(100, lambda = 5),
              x3 = rbinom(100, 10, 0.5),
              letter = sample(letters, 100, replace = TRUE))

# Q4: get sums & number of observations for each letter group
# use tidyverse functions to get those
# output must be tibble format
# for number of observations, use n()

df_sum <- df0 %>% 
  group_by(letter) %>% 
  summarize(sum_x1 = sum(x1),
            sum_x2 = sum(x2),
            sum_x3 = sum(x3),
            n_obs = n())

## calculate mean
# df_sum %>% 
#   mutate(mu_x1 = sum_x1 / n_obs)

# Q5: use pivot_wider
# refer Tidyverse, reshape section

iris_w <- iris %>% 
  mutate(id = rep(1:50, 3)) %>% # add an ID column
  select(id, Sepal.Length, Species) %>% 
  pivot_wider(id_cols = "id", # unique row ID based on
              values_from = "Sepal.Length", # values in each cell from
              names_from = "Species") # new column names from

# Q6: using the following data frame df_na, remove rows with NAs
# use drop_na(x1) in pipe

df_na <- df0 %>% 
  mutate(x1 = ifelse(x1 < 0, NA, x1))

df_na %>% 
  drop_na(x1)


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

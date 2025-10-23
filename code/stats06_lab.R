pacman::p_load(tidyverse,
               patchwork,
               here)
## Regression Lab


# exercise 1 --------------------------------------------------------------


df_setosa <- filter(iris, Species == "setosa")

df_versicolor <- filter(iris, Species == "versicolor")

df_virginica <- filter(iris, Species == "virginica")

m_setosa <- lm(Sepal.Width ~ Petal.Width,
               data = df_setosa)
summary(m_setosa)

m_versicolor <- lm(Sepal.Width ~ Petal.Width,
                   data = df_versicolor)
summary(m_versicolor)

m_virginica <- lm(Sepal.Width ~ Petal.Width,
                  data = df_virginica)
summary(m_virginica)

# exercise 2 --------------------------------------------------------------


m2_setosa <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
                data = df_setosa)

summary(m2_setosa)
summary(m_setosa)

# exercise 3 --------------------------------------------------------------

x <- rnorm(nrow(iris), mean = 0, sd = 1)
iris <- iris %>% 
  mutate(x = x)

dx_setosa <- filter(iris, Species == "setosa")

mx_setosa <- lm(Sepal.Width ~ Petal.Width + x,
                data = dx_setosa)
summary(mx_setosa)
summary(m_setosa)

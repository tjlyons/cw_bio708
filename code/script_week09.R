#' ---
#' title: "Report week9"
#' output: html_document
#' date: "2023-10-12"
#' author: Akira Terui
#' ---

library(tidyverse)

# in class ----------------------------------------------------------------

df_algae <- read_csv(here::here("data_raw/data_algae.csv"))

df_algae %>% 
  ggplot(aes(y = biomass,
             x = conductivity)) +
  geom_point()

# fit linear regression to algae biomass - conductivity

m <- lm(biomass ~ conductivity,
          data = df_algae)

summary(m)

# extract parameters
alpha <- coef(m)[1]
beta <- coef(m)[2]

# draw the line
df_algae %>% 
  ggplot(aes(y = biomass,
             x = conductivity)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta)

# get residuals
y_hat <- alpha + beta * df_algae$conductivity
epsilon <- df_algae$biomass - y_hat

df_algae <- df_algae %>% 
  mutate(y_hat = alpha + beta * conductivity,
         epsilon = biomass - y_hat)

sd(epsilon)

# se estimate
summary(m)

theta <- coef(m)
se <- sqrt(diag(vcov(m)))

t_value <- theta / se

# calculate p-value
p_alpha <- (1 - pt(t_value[1], df = 48)) + pt(-t_value[1], df = 48)

# for slope
p_beta <- (1 - pt(t_value[2], df = 48)) + pt(-t_value[2], df = 48)


# residual
eps <- resid(m)
df_algae <- df_algae %>% 
  mutate(eps = eps)

# visualization of errors
df_algae %>% 
  ggplot(aes(y = biomass,
             x = conductivity)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) +
  geom_segment(aes(x = conductivity,
                   xend = conductivity,
                   y = biomass,
                   yend = biomass - eps),
               linetype = "dashed")


# lab ---------------------------------------------------------------------

## 6.4.1 Develop regression models

### apply filter function separately
df1 <- iris %>% 
  filter(Species == "setosa")

m1 <- lm(Sepal.Width ~ Petal.Width,
         data = df1)

summary(m1)

df2 <- iris %>% 
  filter(Species == "versicolor")

m2 <- lm(Sepal.Width ~ Petal.Width,
         data = df2)

summary(m2)

df3 <- iris %>% 
  filter(Species == "virginica")

m3 <- lm(Sepal.Width ~ Petal.Width,
         data = df3)

summary(m3)

### skip typing species names
sp <- unique(iris$Species)

df1 <- iris %>% 
  filter(Species == sp[1])

df2 <- iris %>% 
  filter(Species == sp[2])

df3 <- iris %>% 
  filter(Species == sp[3])

### lapply
sp <- unique(iris$Species)

list0 <- lapply(X = 1:3, function(i) {
  # i will be substituted by what's in `X`...1, 2, 3
  df_temp <- iris %>% 
    filter(Species == sp[i])
  
  m <- lm(Sepal.Width ~ Petal.Width,
          data = df_temp)
  return(m)
})

list0 <- lapply(X = sp, function(i) {
  # i will be substituted by what's in `X`...setota, versicolor, virginica
  df_temp <- iris %>% 
    filter(Species == i)
  
  m <- lm(Sepal.Width ~ Petal.Width,
          data = df_temp)
  return(m)
})

## 6.4.2 Multiple explanatory variables
### add the second predictor to lm()
### lm(y ~ x1 + x2, ...)

x1 <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
         data = df1)

summary(m1)
summary(x1)

x2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
         data = df2)

summary(m2)
summary(x2)

x3 <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
         data = df3)

summary(m3)
summary(x3)

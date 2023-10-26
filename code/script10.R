#' ---
#' title: "Report week10"
#' output: html_document
#' date: "2023-10-12"
#' author: Akira Terui
#' ---

library(tidyverse)

# read data
df_fl <- read_csv(here::here("data_raw/data_fish_length.csv"))

m <- lm(length ~ lake,
        data = df_fl)


# t-test ------------------------------------------------------------------

# calculate mean for lake a
## base R version
mu_a <- mean(df_fl$length[df_fl$lake == "a"])

## dplyr version
mu_a <- df_fl %>% 
  filter(lake == "a") %>% # pick lake "a"
  pull(length) %>% # pull out column "length", breaking a tibble structure
  mean() # take mean

# calculate mean for lake b
mu_b <- df_fl %>% 
  filter(lake == "b") %>% # pick lake "a"
  pull(length) %>% # pull out column "length", breaking a tibble structure
  mean() # take mean

# diffrence between two groups
b <- mu_b - mu_a

# compare with t-test

## lm summary
summary(m)

## apply t-test
a <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

b <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

t.test(b, a, var.equal = TRUE)


# anova -------------------------------------------------------------------

df_anova <- read_csv(here::here("data_raw/data_fish_length_anova.csv"))

m_anova <- lm(length ~ lake,
              data = df_anova)

# get difference from lake a
df_mu <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  arrange(lake) %>% 
  mutate(diff = mu - mu[1])

v_mu <- pull(df_mu, mu)
names(v_mu) <- pull(df_mu, lake)

## do anova with aov()
m_aov <- aov(length ~ lake,
             data = df_anova)

## compare lm() and aov() output
summary(m_aov)
summary(m_anova)

# combine different types of predictors -----------------------------------

iris <- as_tibble(iris)

m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)

summary(m_iris)


# prediction --------------------------------------------------------------
n_rep <- 100
x <- seq(min(iris$Petal.Width), max(iris$Petal.Width), length = n_rep)
new_x <- rep(x, n_distinct(iris$Species))
new_sp <- rep(unique(iris$Species), each = length(x))

df_new <- tibble(Petal.Width = new_x,
                 Species = new_sp)

df_pred <- df_new %>% 
  mutate(y_hat = predict(m_iris, newdata = .))

g1 <- iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_hat)) # redefine y values for lines; x and color are inherited from ggplot()


# lab ---------------------------------------------------------------------

## 7.3.1 Normality Assumption
eps <- resid(m_iris)
shapiro.test(eps)

## 7.3.2 Model Interpretation
beta <- coef(m_iris)
(i_setosa <- beta[1])
(i_versicolor <- beta[1] + beta[3])
(i_virginica <- beta[1] + beta[4])

## 7.3.3 Alternative Model
m0 <- lm(Petal.Length ~ Petal.Width,
         data = iris)

df_y <- tibble(Petal.Width = seq(min(iris$Petal.Width),
                                 max(iris$Petal.Width),
                                 length = 100)) %>%
  mutate(y_hat = predict(m0, newdata = .))

g0 <- iris %>%
  ggplot(aes(x = Petal.Width,
             y = Petal.Length)) +
  geom_point() +
  geom_line(data = df_y,
            aes(y = y_hat))

library(patchwork)

g0 | g1

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

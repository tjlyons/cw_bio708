pacman::p_load(tidyverse,
               patchwork,
               here)
# Regression

df_algae <- read_csv(here("data_raw/data_algae.csv"))

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point()

m <- lm(biomass ~ conductivity,
        data = df_algae)

summary(m)

alpha <- coef(m)[1]
beta <- coef(m)[2]

df_algae %>% 
  ggplot(aes(y = biomass,
             x = conductivity))+
  geom_point()+
  geom_abline(intercept = alpha,
              slope = beta)

# t-values ----------------------------------------------------------------

se <- sqrt(diag(vcov(m)))

t_value <- beta / se[2]

## p value for slope
(1 - pt(t_value, df = 48)) + pt(-t_value, df = 48)


# coefficient of determination --------------------------------------------

eps <- resid(m)

df_algae <- df_algae %>% 
  mutate(eps = eps) 

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) +
  geom_segment(aes(x = conductivity,
                   xend = conductivity,
                   y = biomass,
                   yend = biomass - eps),
                   linetype = "dashed")

ss <- sum(eps^2)
ss0 <- sum((df_algae$biomass - mean(df_algae$biomass))^2)
## ss/ss0 = 0, good ss/ss0 = 1, bad
r_sq <- 1 - (ss / ss0)

summary(m)

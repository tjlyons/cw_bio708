pacman::p_load(tidyverse,
               patchwork,
               here)
# Generalized Linear Model

# Count data --------------------------------------------------------------

df_count <- read_csv(here("data_raw/data_garden_count.csv"))
view(df_count)

m_normal <- lm(count ~ nitrate,
        data = df_count)
summary(m_normal)

alpha <- coef(m_normal)[1]
beta <- coef(m_normal)[2]

ggplot(df_count) +
  geom_point(aes(x = nitrate,
                 y = count)) +
  geom_abline(intercept = alpha,
              slope = beta)

## random numbers with poisson distribution
y <- rpois(n = 10, lambda = 2)
print(y)

## applying Poisson distribution using glm()

m_pois <- glm(count ~ nitrate,
    data = df_count,
    family = "poisson")
summary(m_pois)

## How not to make a figure using poisson model

alpha_pois <- coef(m_pois)[1]
beta_pois <- coef(m_pois)[2]

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_abline(intercept = alpha_pois,
              slope = beta_pois)

## How to do it right, exponentiate lambda

df_pred <- tibble(nitrate = seq(min(df_count$nitrate),
                     max(df_count$nitrate),
                     length = 100))
y_pred <- predict(m_pois, 
        newdata = df_pred) %>% 
  exp()

df_pred <- df_pred %>% 
  mutate(y = y_pred)

ggplot(df_count,
       aes(x = nitrate,
           y = count)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(y = y))

summary(m_normal)
summary(m_pois)

## binomial distribution

df_mussel <- read_csv(here("data_raw/data_mussel.csv"))
print(df_mussel)


df_mussel <- df_mussel %>% 
  mutate(prop_fert = n_fertilized / n_examined)

df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point()

## cbind() is needed for binomial models (fertilized, unfertilized)
cbind(df_mussel$n_fertilized, df_mussel$n_examined - df_mussel$n_fertilized)

m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
    data = df_mussel,
    family = "binomial")
summary(m_binom)

# logit function
df_test <- tibble(logit_x = seq(-10, 10, length = 100),
       x = (exp(logit_x) / (1 + exp(logit_x))))

#visualizing logit 
df_test %>% 
  ggplot(aes(x = logit_x,
             y = x)) +
  geom_point() +
  geom_line()

#' ---
#' title: "Report week11 GLM"
#' output: html_document
#' date: "2023-10-03"
#' author: Akira Terui
#' ---

#+ message = F, warning = F
library(tidyverse)

# in class: poisson -------------------------------------------------------

## read data
df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))

## try linear model
m_normal <- lm(count ~ nitrate,
               data = df_count)

## draw line
a <- coef(m_normal)[1]
b <- coef(m_normal)[2]

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_abline(intercept = a,
              slope = b)

## try GLM
m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")

summary(m_pois)

theta <- coef(m_pois)
se <- sqrt(diag(vcov(m_pois)))
z_value <- theta / se

## glm prediction

df_pred <- tibble(nitrate = seq(min(df_count$nitrate),
                                max(df_count$nitrate),
                                length = 100)) %>% 
  mutate(y_pois = predict(m_pois, newdata = .) %>% 
           exp(),
         y_norm = predict(m_normal, newdata = .))

df_count %>% 
  ggplot(aes(y = count,
             x = nitrate)) + 
  geom_point() + 
  geom_line(data = df_pred,
            aes(y = y_pois),
            color = "salmon") +
  geom_line(data = df_pred,
            aes(y = y_norm))


# in class: binomial ------------------------------------------------------

df_mussel <- read_csv(here::here("data_raw/data_mussel.csv")) %>% 
  mutate(prop_fert = n_fertilized / n_examined)

# relationship between density and prop eff fertilized
df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point()

# see how logit function works
# x: produce 100 numbers from -100 to 100 (assume logit scale)
# y: convert with inverse-logit transformation (ordinary scale)
df_test <- tibble(logit_p = seq(-10, 10, length = 100),
                  p = exp(logit_p) / (1 + exp(logit_p)))

df_test %>% 
  ggplot(aes(x = logit_p,
             y = p)) +
  geom_point() +
  geom_line() +
  labs(y = "P",
       x = "log(P / 1 - P)")

## use binomial glm
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")

## prediction
df_pred <- tibble(density = seq(min(df_mussel$density),
                                max(df_mussel$density),
                                length = 100)) %>% 
  mutate(logit_y_hat = predict(m_binom, newdata = .),
         y_hat = exp(logit_y_hat) / (1 + exp(logit_y_hat)),
         y_hat0 = boot::inv.logit(logit_y_hat))

## plot fertilization prop data vs. density
## overlay the predicted values from the model

df_mussel %>%
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(y = y_hat))


# lab ---------------------------------------------------------------------

## 8.4.1 GLM exercise
df_fish <- read_csv(here::here("data_raw/data_vpart.csv"))

m <- glm(n_sp ~ distance + cat_area + hull_area,
         data = df_fish,
         family = "poisson")

summary(m)

## check mean variance relationship

mean(df_fish$n_sp)
var(df_fish$n_sp)

## 8.4.2 Effect size
# scaling by SDs
x1 <- rnorm(100, mean = 0, sd = 5)
x2 <- rnorm(100, mean = 0, sd = 10)

sd(x1); sd(x2)

z1 <- x1 / sd(x1)
z2 <- x2 / sd(x2)

sd(z1); sd(z2)

# centering
y1 <- rnorm(100, mean = 10, sd = 5)
y2 <- rnorm(100, mean = 2, sd = 5)

mean(y1); mean(y2)
sd(y1); sd(y2)

mean(scale(y1)); sd(scale(y1))
mean(scale(y2)); sd(scale(y2))


## develop GLM with standardized predictors

### approach 1
m_scl1 <- glm(n_sp ~ scale(distance) + scale(cat_area) + scale(hull_area),
              data = df_fish,
              family = "poisson")

## approach 2
df_fish_scl <- df_fish %>%
  mutate(across(.cols = c("distance", "cat_area", "hull_area"),
                .fns = function(x) c(scale(x))))

m_scl2 <- glm(n_sp ~ distance + cat_area + hull_area,
              data = df_fish_scl,
              family = "poisson")

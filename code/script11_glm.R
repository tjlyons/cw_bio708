#' ---
#' title: "Report week11 GLM"
#' output: html_document
#' date: "2023-10-03"
#' author: Akira Terui
#' ---

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


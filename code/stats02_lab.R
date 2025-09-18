# Sampling Lab

library(tidyverse)
library(patchwork)

# 8.3.1 -------------------------------------------------------------------

df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

mu_h0 <- mean(df_h0$height)
var_h0 <- var(df_h0$height)

mu_i50 <- var_i50 <- NULL

for (i in 1:100) {
  df_i50 <- df_h0 %>% 
    sample_n(size = 50)
  
  (mu_i50[i] <- mean(df_i50$height))
  (var_i50[i] <- sum((df_i50$height - mean(df_i50$height))^2) / nrow(df_i50))
}

mu_i100 <- var_i100 <- NULL

for (i in 1:100) {
  df_i100 <- df_h0 %>% 
    sample_n(size = 100)
  
  (mu_i100[i] <- mean(df_i100$height))
  (var_i100[i] <- sum((df_i100$height - mean(df_i100$height))^2) / nrow(df_i100))
}

# Creating mu/var histograms for df_h0[50] and df_h0[100]

h0_est <- tibble(mu_i50 = mu_i50,
                 mu_i100 = mu_i100,
                 var_i50 = var_i50,
                 var_i100 = var_i100)

mu_h0_i50 <- h0_est %>% 
  ggplot(aes(x = mu_i50))+
  geom_histogram()+
  geom_vline(xintercept = mu_h0)

mu_h0_i100 <- h0_est %>% 
  ggplot(aes(x = mu_i100))+
  geom_histogram()+
  geom_vline(xintercept = mu_h0)

var_h0_i50 <- h0_est %>% 
  ggplot(aes(x = var_i50))+
  geom_histogram()+
  geom_vline(xintercept = var_h0)

var_h0_i100 <- h0_est %>% 
  ggplot(aes(x = var_i100))+
  geom_histogram()+
  geom_vline(xintercept = var_h0)

# Comparing mu/var histograms for df_h0[50] and df_h0[100]

h0_mu_p1 <- (mu_h0_i50 / mu_h0_i100) +
  plot_layout(ncol = 2)

h0_var_p2 <- (var_h0_i50 / var_h0_i100) +
  plot_layout(ncol = 2)

h0_est_p3 <- h0_mu_p1 / h0_var_p2

# 8.3.2 -------------------------------------------------------------------

df_h10 <- df_h0 %>% 
  filter(height >= 10)

mu_h10 <- mean(df_h10$height)
var_h10 <- var(df_h10$height)

mu_10i50 <- var_10i50 <- NULL

for (i in 1:100) {
  df_10i50 <- df_h10 %>% 
    sample_n(size = 100)
  
  (mu_10i50[i] <- mean(df_10i50$height))
  (var_10i50[i] <- sum((df_10i50$height - mean(df_10i50$height))^2) / nrow(df_10i50))
  mu_h10 <- mean(df_h10$height)
  var_h10 <- var(df_h10$height)
}

mu_10i50 <- var_10i50 <- NULL

for (i in 1:100) {
  df_10i50 <- df_h10 %>% 
    sample_n(size = 50)
  
  (mu_10i50[i] <- mean(df_10i50$height))
  (var_10i50[i] <- sum((df_10i50$height - mean(df_10i50$height))^2) / nrow(df_10i50))
}

mu_10i100 <- var_10i100 <- NULL

for (i in 1:100) {
  df_10i100 <- df_h10 %>% 
    sample_n(size = 100)
  
  (mu_10i100[i] <- mean(df_10i100$height))
  (var_10i100[i] <- sum((df_10i100$height - mean(df_10i100$height))^2) / nrow(df_10i100))
}

# Creating mu/var histograms for df_h10[50] and df_h10[100]

h10_est <- tibble(mu_i50 = mu_10i50,
                  mu_i100 = mu_10i100,
                  var_i50 = var_10i50,
                  var_i100 = var_10i100)

mu_h10_i50 <- h10_est %>% 
  ggplot(aes(x = mu_i50))+
  geom_histogram()+
  geom_vline(xintercept = mu_h10)

mu_h10_i100 <- h10_est %>% 
  ggplot(aes(x = mu_i100))+
  geom_histogram()+
  geom_vline(xintercept = mu_h10)

var_h10_i50 <- h10_est %>% 
  ggplot(aes(x = var_i50))+
  geom_histogram()+
  geom_vline(xintercept = var_h10)

var_h10_i100 <- h10_est %>% 
  ggplot(aes(x = var_i100))+
  geom_histogram()+
  geom_vline(xintercept = var_h10)

# Comparing mu/var histograms for df_h10[50] and df_h10[100]

h10_mu_p1 <- (mu_h10_i50 / mu_h10_i100) +
  plot_layout(ncol = 2)

h10_var_p2 <- (var_h10_i50 / var_h10_i100) +
  plot_layout(ncol = 2)

h10_est_p4 <- h10_mu_p1 / h10_var_p2

# Comparing all mu/var histograms

h0_est_p3 / h10_est_p4

#' ---
#' title: "Report week4"
#' output: html_document
#' date: "2023-09-12"
#' author: Akira Terui
#' ---

#+ message = F, warning = F
library(tidyverse)

# in class ----------------------------------------------------------------
# pland data frame
h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h1 <- tibble(plant_id = 1:10, # a vector from 1 to 10 by 1
                height = h, # height
                unit = "cm") # unit

# nrow() returns the number of rows
# while piping, "." refers to the dataframe inherited 
# i.e., nrow(.) counts the number of rows in df_h1
df_h1 <- df_h1 %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h1)

# another set of 10
h <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h2)

# 1000 plants
# load csv data on R
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

# show the first 10 rows
print(df_h0)

# summary statistics
mu <- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

print(mu)
print(sigma2)

# subsample with sample_n()
df_i <- df_h0 %>% 
  sample_n(size = 10) # size specifies the number of rows to be selected randomly

print(df_i)

# subsample replicates
# for reproducibility
set.seed(3)

mu_i <- var_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
}

# histogram

#install.packages("patchwork") # install only once
library(patchwork)

df_sample <- tibble(mu_hat = mu_i, var_hat = var_i)

# histogram for mean
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

# histogram for variance
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

# layout vertically
# possible only if "patchwork" is loaded
g_mu / g_var

# unbiased variance
# for reproducibility
set.seed(3)

# redo simulations ----
mu_i <- var_i <- var_ub_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
  var_ub_i[i] <- var(df_i$height)
}

# histogram again

# draw histograms ----
df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i,
                    var_ub_hat = var_ub_i)

# histogram for mu
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

# histogram for variance
# scale_x_continuous() adjusts scale in x-axis
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

# histogram for unbiased variance
g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

g_mu / g_var / g_var_ub


# laboratory --------------------------------------------------------------

# Q1 get 50 & 100 measures each

## declare some essential objects
var50 <- var100 <- mu50 <- mu100 <- NULL
subsize <- 100

## loop over
for(i in 1:subsize) {
  df_i50 <- df_h0 %>% 
    sample_n(size = 50)
  
  df_i100 <- df_h0 %>% 
    sample_n(size = 100)
  
  mu50[i] <- mean(df_i50$height)  
  mu100[i] <- mean(df_i100$height)  
  var50[i] <- var(df_i50$height)  
  var100[i] <- var(df_i100$height)  
}

## create tibble
df_par <- tibble(value = c(mu50, mu100, var50, var100),
                 type = c(rep("mean", 2 * subsize),
                          rep("var", 2 * subsize)),
                 size = c(rep(50, subsize),
                          rep(100, subsize),
                          rep(50, subsize),
                          rep(100, subsize)),
                 true_par = ifelse(type == "mean", mu, sigma2))

## histogram
df_par %>% 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_grid(rows = vars(size),
             cols = vars(type),
             scales = "free",
             labeller = label_both) +
  geom_vline(aes(xintercept = true_par)) +
  theme_bw()

# Q2 non-random samples

df_h10 <- df_h0 %>% 
  filter(height >= 10)

## declare some essential objects
var50 <- var100 <- mu50 <- mu100 <- NULL
subsize <- 100

## loop over
for(i in 1:subsize) {
  df_i50 <- df_h10 %>% 
    sample_n(size = 50)
  
  df_i100 <- df_h0 %>% 
    sample_n(size = 100)
  
  mu50[i] <- mean(df_i50$height)  
  mu100[i] <- mean(df_i100$height)  
  var50[i] <- var(df_i50$height)  
  var100[i] <- var(df_i100$height)  
}

## create tibble
df_par10 <- tibble(value = c(mu50, mu100, var50, var100),
                   type = c(rep("mean", 2 * subsize),
                            rep("var", 2 * subsize)),
                   size = c(rep(50, subsize),
                            rep(100, subsize),
                            rep(50, subsize),
                            rep(100, subsize)),
                   true_par = ifelse(type == "mean", mu, sigma2))

df_par10 %>% 
  ggplot(aes(x = value)) +
  geom_histogram(fill = "salmon") +
  facet_grid(rows = vars(size),
             cols = vars(type),
             scales = "free",
             labeller = label_both) +
  geom_vline(aes(xintercept = true_par)) +
  theme_bw()

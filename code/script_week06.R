#' ---
#' title: "Report week6"
#' output: html_document
#' date: "2023-09-19"
#' author: Akira Terui
#' ---

#+ message = F, warning = F
library(tidyverse)

# continuous --------------------------------------------------------------

# load csv data on R
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1) + # specify binwidth
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean

# probability distribution

# vector of x values
# seq() generate min to max values with specified numbers of elements or interval
# the following produce 100 elements
x <- seq(min(df_h0$height), max(df_h0$height), length = 100)

# calculate probability density
mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)

# figure
tibble(y = pd, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line() + # draw lines
  labs(y = "Probability density") # re-label

# density to probability
# probability of x < 10
p10 <- pnorm(q = 10, mean = mu, sd = sigma)
print(p10)

# probability of x < 20
p20 <- pnorm(q = 20, mean = mu, sd = sigma)
print(p20)

# probability of 10 < x < 20
p20_10 <- p20 - p10
print(p20_10)

# probability histogram
x_min <- floor(min(df_h0$height)) # floor takes the integer part of the value
x_max <- ceiling(max(df_h0$height)) # ceiling takes the next closest integer
bin <- seq(x_min, x_max, by = 1) # each bin has 1cm

p <- NULL # empty object for probability
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) - pnorm(bin[i], mean = mu, sd = sigma)
}

# data frame for probability
# bin: last element [-length(bin)] was removed to match length
# expected frequency in each bin is "prob times sample size"
df_prob <- tibble(p, bin = bin[-length(bin)]) %>% 
  mutate(freq = p * nrow(df_h0))

# expectation vs. observation
df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1) +
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "salmon") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "salmon")


# discrete ----------------------------------------------------------------

# data
df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))
print(df_count)

# histogram
df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # define binwidth
                 center = 0) # relative position of each bin

# vector of x values
# create a vector of 0 to 10 with an interval one
# must be integer of > 0
x <- seq(0, 10, by = 1)

# calculate probability mass
lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)

# figure
tibble(y = pm, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line(linetype = "dashed") + # draw dashed lines
  geom_point() + # draw points
  labs(y = "Probability",
       x = "Count") # re-label

# expectation vs. observation
df_prob <- tibble(x = x, y = pm) %>% 
  mutate(freq = y * nrow(df_count)) # prob x sample size

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5,
                 center = 0) +
  geom_line(data = df_prob,
            aes(x = x,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob,
             aes(x = x,
                 y = freq))


# laboratory --------------------------------------------------------------

rm(list = ls())

# 1. normal distribution

## 1.1 generate a random variable
z <- rnorm(n = 50, mean = 100, sd = 5)
mu <- mean(z)
sigma <- sd(z)

## 1.2 generate a figure
bin <- seq(floor(min(z)), ceiling(max(z)), by = 1)

p <- NULL
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i + 1], mean = mu, sd = sigma) - pnorm(bin[i], mean = mu, sd = sigma)
}

df_norm <- tibble(p = p, x = bin[-length(bin)]) %>% 
  mutate(freq = p * length(z))

tibble(z = z) %>% 
  ggplot(aes(x = z)) +
  geom_histogram(binwidth = 1,
                 center = 0) +
  geom_point(data = df_norm,
             aes(x = x,
                 y = freq)) + 
  geom_line(data = df_norm,
            aes(x = x,
                y = freq))

# 2. Poisson distribution

## 2.1 generate a random variable
k <- rpois(n = 1000, lambda = 5)
lambda_hat <- mean(k)

min_k <- min(k)
max_k <- max(k)
k_at <- seq(min_k, max_k, by = 1)

## 2.2 generate a figure
df_pois <- tibble(x = k_at,
                  pm = dpois(k_at, lambda = lambda_hat)) %>% 
  mutate(freq = length(k) * pm)

tibble(k = k) %>% 
  ggplot() +
  geom_histogram(aes(x = k)) +
  geom_point(data = df_pois,
             aes(x = x,
                 y = freq)) +
  geom_line(data = df_pois,
            aes(x = x,
                y = freq))

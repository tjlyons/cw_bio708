library(tidyverse)
library(patchwork)
# Probability lab

# Normal Distribution -----------------------------------------------------

a <- rnorm(50, mean = 50, sd = 5)

df_a <- tibble(x = a)

mu_a <- mean(a)
sigma_a <- sd(a)
pd_a <- dnorm(a, mean = mu_a, sd = sigma_a)

h_a <- df_a %>%
  ggplot(aes(x = a))+
  geom_histogram()

hpd_a <- tibble(y = pd_a,
       x = a) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  labs(y = "Probability Density",
       x = "Value")

a_min <- floor(min(a)) 
a_max <- ceiling(max(a)) 
bin <- seq(a_min, a_max, by = 1) 

p <- NULL 
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mu_a, sd = sigma_a) - pnorm(bin[i], mean = mu_a, sd = sigma_a)
}

df_prob <- tibble(bin = bin[-length(bin)],
                   prob = p) %>% 
  mutate(freq = length(a) * prob)

df_a %>% 
  ggplot(aes(x = x)) + 
  geom_histogram(binwidth = 1, 
                 center = 0.5) + 
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "purple") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "pink")

# Poisson Distribution ----------------------------------------------------

x <- rpois(n = 1000, lambda = 5)
lambda_hat <- mean(x)
pm <- dpois(x, lambda = lambda_hat)

# Probability Histogram
h_prob_x <- tibble(y = pm, x = x) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line(linetype = "dashed") + 
  geom_point() + 
  labs(y = "Probability",
       x = "Count") 

df_count <- tibble(count = x)

df_prob_x <- tibble(x = x, y = pm) %>% 
  mutate(freq = y * nrow(df_count)) 

# Frequency overlayed with samples
df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, 
                 center = 0) +
  geom_line(data = df_prob_x,
            aes(x = x,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob_x,
             aes(x = x,
                y = freq))

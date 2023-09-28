#' ---
#' title: "Report week7"
#' output: html_document
#' date: "2023-09-26"
#' author: Akira Terui
#' ---

# class -------------------------------------------------------------------

#+ message = F, warning = F
library(tidyverse)

df_fl <- read_csv(here::here("data_raw/data_fish_length.csv"))
print(df_fl)

# unique returns unique values as a vector
unique(df_fl$lake)

# distinct returns unique values as a tibble
distinct(df_fl, lake)

# group mean and sd
df_fl_mu <- df_fl %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), # summarize by mean()
            sd_l = sd(length)) # summarize with sd()

# plot
# geom_jitter() plot data points with scatter
# geom_segment() draw lines
# geom_point() draw points
df_fl %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_jitter(width = 0.1, # scatter width
              height = 0, # scatter height (no scatter with zero)
              alpha = 0.25) + # transparency of data points
  geom_segment(data = df_fl_mu, # switch data frame
               aes(x = lake,
                   xend = lake,
                   y = mu_l - sd_l,
                   yend = mu_l + sd_l)) +
  geom_point(data = df_fl_mu, # switch data frame
             aes(x = lake,
                 y = mu_l),
             size = 3) +
  labs(x = "Lake", # x label
       y = "Fish body length") # y label

# take another look at df_fl_mu
print(df_fl_mu)

# pull mu_l from tibble as vector
v_mu <- df_fl_mu %>% 
  pull(mu_l)

# lake a
print(v_mu[1])

# lake b
print(v_mu[2])

# difference
v_mu[1] - v_mu[2]

# group mean, variance, and sample size
df_t <- df_fl %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), # summarize by mean()
            var_l = var(length), # summarize with sd()
            n = n()) # count number of rows per group

print(df_t)

# pull values as a vector
v_mu <- pull(df_t, mu_l)
v_var <- pull(df_t, var_l)
v_n <- pull(df_t, n)

var_p <- ((v_n[1] - 1)/(sum(v_n) - 2)) * v_var[1] +
  ((v_n[2] - 1)/(sum(v_n) - 2)) * v_var[2]

t_value <- (v_mu[1] - v_mu[2]) / sqrt(var_p * ((1 / v_n[1]) + (1 / v_n[2])))

print(t_value)

# produce 500 values from -5 to 5 with equal interval
x <- seq(-5, 5, length = 500)

# probability density of t-statistics with df = sum(v_n) - 2
y <- dt(x, df = sum(v_n) - 2)

# draw figure
tibble(x, y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  labs(y = "Probability density",
       x = "t-statistic")

# draw entire range
tibble(x, y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_vline(xintercept = t_value,
             color = "salmon") + # t_value is the observed t_value
  geom_vline(xintercept = abs(t_value),
             color = "salmon") + # t_value is the observed t_value
  labs(y = "Probability density",
       x = "t-statistic") 

# calculate area under the curve from -infinity to t_value
pr_below <- pt(q = t_value, df = sum(v_n) - 2)

# calculate area under the curve from abs(t_value) to infinity
pr_above <- 1 - pt(q = abs(t_value), df = sum(v_n) - 2)

# p_value
p_value <- pr_below + pr_above
print(p_value)

# t-test in R
x <- df_fl %>%
  filter(lake == "a") %>%  # subset lake a
  pull(length)

y <- df_fl %>%
  filter(lake == "b") %>% # subset lake b
  pull(length)

t.test(x, y, var.equal = TRUE)


# laboratory --------------------------------------------------------------

library(tidyverse)

# exercise 1: Influence of Sample Size

## create vactors
xs <- rnorm(10, mean = 10, sd = 5)
ys <- rnorm(10, mean = 12, sd = 5)

xl <- rnorm(100, mean = 10, sd = 5)
yl <- rnorm(100, mean = 12, sd = 5)

## t.test
### small sample size
### var.equal true assumption
t.test(xs, ys, var.equal = TRUE)

### var.equal false assumption
t.test(xs, ys, var.equal = FALSE)

### large sample size
### var.equal true assumption
t.test(xl, yl, var.equal = TRUE)

### var.equal false assumption
t.test(xl, yl, var.equal = FALSE)


# exercise 2: Difference and Uncertainty

a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

# approach 1
df1 <- tibble(value = c(a1, a2, b1, b2),
       group = rep(c("a1", "a2", "b1", "b2"),
                   each = length(a1))) %>% 
  group_by(group) %>% 
  summarize(mu = mean(value),
            sd = sd(value))

# approach 2
df2 <- tibble(a1, a2, b1, b2) %>% 
  pivot_longer(cols = everything(),
               values_to = "value",
               names_to = "group") %>% 
  group_by(group) %>% 
  summarize(mu = mean(value),
            sd = sd(value))

t.test(a1, a2)
t.test(b1, b2)

# exercise 3: welch's t-test
x <- rnorm(50, mean = 10, sd = 1)
y <- rnorm(50, mean = 10, sd = 2)   

mu_x <- mean(x)
mu_y <- mean(y)

sd_x <- sd(x)
sd_y <- sd(y)

# t-value for welch's test
a <- (sd_x^2 / length(x)) + (sd_y^2 / length(y))
t_value <- (mu_x - mu_y) / sqrt(a)

# degree of freedom for welch's test
num <- var(x) / length(x) + var(y) / length(y)
dem1 <- ((var(x) / length(x))^2) / (length(x) - 1)
dem2 <- ((var(y) / length(y))^2) / (length(y) - 1)

df_welch <- num^2 / (dem1 + dem2)
p_value <- (1 - pt(abs(t_value), df = df_welch)) * 2

print(t_value)
print(df_welch)
print(p_value)
t.test(x, y, var.equal = FALSE)

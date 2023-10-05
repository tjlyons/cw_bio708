#' ---
#' title: "Report week8"
#' output: html_document
#' date: "2023-10-03"
#' author: Akira Terui
#' ---

# class work --------------------------------------------------------------

#+ warning = F, message = F
library(tidyverse)

## read data
df_anova <- read_csv(here::here("data_raw/data_fish_length_anova.csv"))

### check
distinct(df_anova, lake)

df_anova %>% 
  distinct(lake)

unique(df_anova$lake)

### figure
df_anova %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_violin(draw_quantiles = 0.5,
              fill = "salmon",
              alpha = 0.5) +
  geom_jitter(width = 0.2,
              height = 0,
              alpha = 0.5)

df_anova %>% 
  ggplot(aes(y = lake,
             x = length)) +
  geom_violin(draw_quantiles = 0.5,
              fill = "salmon",
              alpha = 0.5) +
  geom_jitter(width = 0,
              height = 0.2,
              alpha = 0.5)

### anova in R
fit <- aov(formula = length ~ lake,
           data = df_anova)

summary(fit)


## behind anova

### between group variability

### group-level information
x_a <- df_anova %>%
  filter(lake == "a") %>%
  pull(length)

x_b <- df_anova %>%
  filter(lake == "b") %>%
  pull(length)

x_c <- df_anova %>%
  filter(lake == "c") %>%
  pull(length)

mu_a <- mean(x_a)
mu_b <- mean(x_b)
mu_c <- mean(x_c)

# ### variability
# ss_ga <- (mu_a - mu)^2 * length(x_a)
# ss_gb <- (mu_b - mu)^2 * length(x_b)
# ss_gc <- (mu_c - mu)^2 * length(x_c)
# 
# ss_g <- ss_ga + ss_gb + ss_gc

### estimate overall mean
mu <- mean(df_anova$length)

### estimate group means and sample size each
df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length), # mean for each group
            dev_g = (mu_g - mu)^2, # squared deviation for each group
            n = n()) # sample size for each group

ss_g <- df_g %>% 
  mutate(ss = dev_g * n) %>% 
  pull(ss) %>% 
  sum()

### within-group variability
dev_ia <- (x_a - mu_a)^2
dev_ib <- (x_b - mu_b)^2
dev_ic <- (x_c - mu_c)^2

ss_w <- sum(dev_ia) + sum(dev_ib) + sum(dev_ic)

# df_i <- df_anova %>% 
#   group_by(lake) %>% 
#   mutate(mu_g = mean(length)) %>% # use mutate() to retain individual rows
#   ungroup() %>% 
#   mutate(dev_i = (length - mu_g)^2) # deviation from group mean for each fish

### convert variability into SD
var_g <- ss_g / (3 - 1)
var_w <- ss_w / (150 - 3)

### get F value
f_value <- var_g / var_w


### f distribution 

x <- seq(0, 10, by = 0.1)
y <- df(x, df1 = 3 - 1, df2 = 150 - 3)

tibble(x = x, y = y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_vline(xintercept = f_value,
             color = "salmon",
             linetype = "dashed")

p_value <- 1 - pf(f_value, df1 = 3 - 1, df2 = 150 - 3)


# laboratory --------------------------------------------------------------

library(tidyverse)

## exercise 1 ####
# use PlantGrowth dataset
# create figure like 5.1
df_pg <- as_tibble(PlantGrowth)

g_vio <- df_pg %>% 
  ggplot(aes(x = group,
             y = weight)) +
  geom_violin(draw_quantiles = 0.5,
              fill = "steelblue",
              alpha = 0.25) +
  geom_jitter(width = 0.1,
              alpha = 0.25) +
  theme_bw() +
  labs(y = "Weight",
       x = "Group")

# create figure like 4.1

df_mu <- df_pg %>% 
  group_by(group) %>% 
  summarize(mu = mean(weight),
            sigma = sd(weight))

g_bar <- df_mu %>% 
  ggplot(aes(x = group,
             y = mu)) +
  geom_segment(aes(y = mu - sigma,
                   yend = mu + sigma,
                   x = group,
                   xend = group)) +
  geom_point(size = 4) +
  geom_jitter(data = df_pg,
              aes(x = group,
                  y = weight),
              width = 0.1,
              alpha = 0.25)
  
## exercise 2 ####
# anova
fit <- aov(formula = weight ~ group,
           data = df_pg)

summary(fit)

## exercise 3 ####
# try different numbers of df1 and df2
# 1) we have 5 groups with 30 measures
# df1 = 5 - 1
# df2 = 30 - 5
# use pf() and f_value = 4.846

1 - pf(4.846, df1 = 4, df2 = 25)

# 2) we have 3 groups with 15 measures

1 - pf(4.846, df1 = 2, df2 = 12)

# 3) we have 4 groups with 20 measures

1 - pf(4.846, df1 = 3, df2 = 16)

# visual for different f distributions
nx <- 1000
x <- seq(0, 10, length = nx)
df_y <- tibble(y = c(df(x, df1 = 4, df2 = 25),
                     df(x, df1 = 2, df2 = 12),
                     df(x, df1 = 3, df2 = 16))) %>% 
  mutate(x = rep(x, 3),
         df1 = rep(c(4, 2, 3), each = nx),
         df2 = rep(c(25, 12, 16), each = nx),
         df = paste(df1, df2, sep = ","))

df_y %>% 
  ggplot(aes(y = y,
             x = x,
             color = df)) +
  geom_line()


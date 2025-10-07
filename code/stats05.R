pacman::p_load(tidyverse,
               patchwork,
               here)
# ANOVA

df_anova <- read_csv(here("data_raw/data_fish_length_anova.csv"))
distinct(df_anova, lake)

colnames(df_anova)

## visuals
df_anova %>% 
  ggplot(aes(x = lake,
             y = length))+
  geom_violin(draw_quantiles = 0.5,
              alpha = 0.2,
              fill = "steelblue") +
  geom_jitter(width = 0.1) +
  theme_bw()


# perform anova ------------------------------------------------------------

# formula: y ~ x (response on left, predictor on right)
m <- aov(length ~ lake, 
         data = df_anova)

summary(m)

# between group variability -----------------------------------------------

mu <- mean(df_anova$length)

s_b <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length),
            n = n()) %>% 
  mutate(dev_g = (mu_g - mu)^2,
         ss_g = dev_g * n) %>% 
  pull(ss_g) %>% 
  sum()

# within group variability ------------------------------------------------

s_w <- df_anova %>% 
  group_by(lake) %>% 
  mutate(mu_g = mean(length)) %>% 
  ungroup() %>% 
  mutate(dev_i = (length - mu_g)^2) %>% 
  pull(dev_i) %>% 
  sum()

# summed square to values -------------------------------------------------

s2_b <- s_b / (n_distinct(df_anova$lake) - 1)
s2_w <- s_w / (nrow(df_anova) - n_distinct(df_anova$lake))


# f statistic -------------------------------------------------------------

f_value <- s2_b / s2_w

f_null <- seq(0, 10, by = 0.1)
y <- df(x = f_null, 
        df1 = 2,
        df2 = 147)               

tibble(x = f_null,
       y = y) %>% 
  ggplot(aes(x = x,
             y = y))+
  geom_line() +
  geom_vline(xintercept = f_value,
             color = "pink")

p_value <- 1 - pf(q = f_value,
                  df1 = 2,
                  df2 = 147)


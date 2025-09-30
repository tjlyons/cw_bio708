#install.packages("pacman")
pacman::p_load(tidyverse,
              patchwork,
              here)
# Two group comparison (t-test)

df_fl <- read_csv(here::here("data_raw/data_fish_length.csv")) 
print(df_fl)

unique(df_fl$lake)
distinct(df_fl, lake)

df_fl_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu_l = mean(length),
            sd_l = sd(length))

df_fl %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_jitter(width = 0.1,
              alpha = 0.25) +
  geom_segment(data = df_fl_mu,
               aes(x = lake,
                   xend = lake,
                   y = mu_l - sd_l,
                   yend = mu_l + sd_l)) +
  geom_point(data = df_fl_mu,
             aes(x = lake,
                 y = mu_l)) +
  labs(x = "Lake",
       y = "Fish Length")


## T-test
x <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

y <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

t.test(x, y, var.equal = TRUE)

# details in t test -------------------------------------------------------

mu_x <- mean(x)
mu_y <- mean(y)
mu_x - mu_y

# key statistics
df_t <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu_l = mean(length),
            var_l = var(length),
            n = n())

v_mu <- df_t %>% 
  pull(mu_l)
v_var <- df_t %>% 
  pull(var_l)
v_n <- df_t %>% 
  pull(n)

# manual t statistic
var_a <- ((v_n[1] - 1) / (v_n[1] + v_n[2] - 2)) * v_var[1]
var_b <- ((v_n[2] - 1) / (v_n[1] + v_n[2] - 2)) * v_var[2]
var_p <- var_a + var_b

t_value <- (v_mu[1] - v_mu[2]) / sqrt((var_p) * ((1 / v_n[1]) + (1 / v_n[2])))

# null distribution
x <- seq(-5, 5, length = 500)

y <- dt(x, df = 98)

tibble(x, y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  labs(y = "Probability Density",
       x = "t-statistic") +
  geom_vline(xintercept = t_value) +
  geom_vline(xintercept = abs(t_value))

p_lower <- pt(q = t_value, df = 98)
p_higher <- 1 - pt(q = abs(t_value), df = 98)

p_value <- p_lower + p_higher

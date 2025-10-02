pacman::p_load(tidyverse,
               patchwork,
               here)
## T-test lab

xs <- rnorm(n = 10, mean = 10, sd = 5)
ys <- rnorm(n = 10, mean = 12, sd = 5)
x1 <- rnorm(n = 100, mean = 10, sd = 5)
y1 <- rnorm(n = 100, mean = 12, sd = 5)

t.test(xs, ys, var.equal = TRUE)
t.test(x1, y1, var.equal = TRUE)

a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

df_0 <- tibble(group = c(rep("a1", length(a1)),
                         rep("a2", length(a2)),
                         rep("b1", length(b2)),
                         rep("b2", length(b2))),
               value = c(a1, a2, b1, b2))
                                        

stats_0 <- df_0 %>% 
  group_by(group) %>% 
  summarize(mu = mean(value),
            sd = sd(value))


df_filt <- df_0 %>% 
  filter(group %in% c("a1", "a2"))
stats_filt <- stats_0 %>% 
  filter(group %in% c("a1", "a2"))

df_filt %>% 
  ggplot(aes(x = group,
             y = value)) +
  geom_jitter(width = 0.1,
              alpha = 0.25) +
  geom_segment(data = stats_filt,
               aes(x = group,
                   xend = group,
                   y = mu - sd,
                   yend = mu + sd)) +
  geom_point(data = stats_filt,
             aes(x = group,
                 y = mu)) +
  labs(x = "Group",
       y = "Value")

t.test(a1, a2, var.equal = TRUE)
t.test(b1, b2, var.equal = TRUE)

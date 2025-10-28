pacman::p_load(tidyverse,
               patchwork,
               here)
## Linear Models


# t-test versus lm --------------------------------------------------------


df_fl <- read_csv(here("data_raw/data_fish_length.csv"))
print(df_fl)

m <- lm(length ~ lake,
   data = df_fl)
summary(m)

v_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu_l = mean(length)) %>% 
  pull(mu_l)

# mean lengths for lakes & the difference between
v_mu[1]
v_mu[2]
v_mu[2] - v_mu[1]

# mean length for lake b (repeating previous step backwards)
sum(coef(m)) 

a <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

b <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

t.test(x = a, y = b, var.equal = TRUE)
summary(m)

## anova vs lm

df_anova <- read_csv(here("data_raw/data_fish_length_anova.csv"))
print(df_anova)

m1 <- lm(length ~ lake,
         data = df_anova)
summary(m1)

mu_anova <- df_anova %>%  
  group_by(lake) %>% 
  summarize(mu_l = mean(length)) %>% 
  pull(mu_l)

mu_anova[1] ## intercept

mu_anova[2] - mu_anova[1] ## lake b effect

mu_anova[3] - mu_anova[1] ## lake c effect


aov <- aov(length ~ lake,
           data = df_anova)
summary(aov)


# ancova ------------------------------------------------------------------
##analysis of covariance, accounts for categorical AND numerical groups

m2 <- lm(Sepal.Length ~ Sepal.Width + Species,
   data = iris) %>% 
  summary()

## visualizing ANCOVA

m_iris <- lm(Petal.Length ~ Petal.Width + Species,
         data = iris)
summary(m_iris)

n_rep <- 100
df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                        max(iris$Petal.Width),
                                        length = n_rep),
                                    n_distinct(iris$Species)),
                  Species = rep(unique(iris$Species),
                                each = n_rep))

y_pred <- predict(m_iris,
                  newdata = df_pred)
       
df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

## visual output

ggplot(iris,
       aes(x = Petal.Width,
           y = Petal.Length,
           color = Species)) + 
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(x = Petal.Width,
                y = y_pred))



# interaction -------------------------------------------------------------
# including into lm

# asterisk = interaction
m_int <- lm(Petal.Length ~ Petal.Width * Species,
   data = iris)
summary(m_int)


## or you can write it the long way
lm(Petal.Length ~ Petal.Width + Species + Petal.Width:Species,
   data = iris) %>% 
  summary()

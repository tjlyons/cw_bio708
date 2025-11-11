pacman::p_load(tidyverse,
               patchwork,
               here)
## Likelihood

set.seed(1)

# hypothetical sample size
n <- 100

# true intercept and slope
b <- c(0.1, 0.5)

# simulated predictor
x1 <- rnorm(n = n, mean = 0, sd = 1)

# design matrix
X <- model.matrix(~ x1)

# simulating y
y_hat <- X %*% b

y <- rnorm(n = n, mean = y_hat, sd = 0.5)

df_0 <- tibble(y = y,
       x1 = x1)

df_0 %>% 
  ggplot(aes(x = x1,
             y = y)) +
  geom_point()

# fitting the model to simulated data
lm(y ~ x1,
   data = df_0) %>% 
  summary()


# likelihood --------------------------------------------------------------

#probability of observing 3 with lambada = 3 under poisson
dpois(3, lambda = 3.5)
dpois(1, lambda = 3.5)
dpois(4, lambda = 3.5)
dpois(10, lambda = 3.5)

lambda <- seq(0, 10, by = 0.1)
pr <- dpois(3, lambda = lambda)

df_pois <- tibble(y = 3, 
       lambda = lambda,
       pr = pr)

df_pois %>% 
  ggplot(aes(x = lambda,
             y =  pr)) +
  geom_point() +
  geom_line() +
  labs(x = "lambda",
       y = "Pr(y = 3)")
         
df_pois %>% 
  arrange(desc(pr))

# probability of observing 3, 2, and 5 at the same time

pr <- dpois(c(3,2,5), lambda = 3)
prod(pr)

# lambda for 3, 2, 5
y <- c(3, 2, 5)
lambda <- seq(0, 10, by = 0.01)

pr <- sapply(X = lambda,
             FUN = function(z) prod(dpois(y, lambda = z)))

df_pois <- tibble(lambda = lambda,
                  pr = pr)

df_pois %>% 
  ggplot(aes(x = lambda,
             y =  pr)) +
  geom_point() +
  geom_line() +
  labs(x = "lambda",
       y = "Pr(y = 3, 2, 5)")

df_pois %>% 
  arrange(desc(pr))

mean(c(3,2,5))


df_count <- read.csv(here("data_raw/data_garden_count.csv"))
m_pois <- glm(count ~ nitrate,
    data = df_count,
    family = "poisson")

summary(m_pois)

logLik(m_pois)


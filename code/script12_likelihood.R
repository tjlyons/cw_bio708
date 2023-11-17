#' ---
#' title: "Report week12 Likelihood"
#' output: html_document
#' date: "2023-11-07"
#' author: Akira Terui
#' ---

#+ message = F, warning = F
library(tidyverse)

# calculate the probablity of y = 3
# with lambda = 3.5

pr <- dpois(3, lambda = 3.5)

# write the formula to confirm
(p <- (3.5^3 * exp(-3.5)) / factorial(3))

# possible lambda values
lambda <- seq(0, 10, by = 0.01)

df_pr <- tibble(y = 3,
                lambda = lambda,
                pr = dpois(3, lambda = lambda))

# draw figure, y = pr, x = lambda
# ggplot()

df_pr %>% 
  ggplot(aes(x = lambda,
             y = pr)) +
  geom_line()

# find best lambda
df_pr %>% 
  arrange(desc(pr))

# calculate probability of observing y = c(3, 2, 5)
# lambda = 3

pr <- prod(dpois(c(3, 2, 5), lambda = 3))
prod(pr)

# find the best lambda for y = c(3, 2, 5)

## approach 1: use for {} loop
likeli <- NULL
for (i in 1:length(lambda)) {
  pr0 <- dpois(c(3, 2, 5), lambda = lambda[i])
  likeli[i] <- prod(pr0)
}

## figure
df_pr1 <- tibble(lambda = lambda, pr = likeli)
df_pr1 %>% 
  ggplot(aes(x = lambda,
             y = pr)) +
  geom_line()

## find the best value of lambda using df_pr1
df_pr1 %>% 
  arrange(desc(pr)) %>% 
  slice(1) %>% # retrieve row 1
  pull(lambda) # pull out lambda column as a vector/scalar

mean(c(3, 2, 5))


# lab ---------------------------------------------------------------------

## 9.3.1 Binomial Distribution

### Q1
#### data and possible p
y <- c(2, 2, 0, 0, 3, 1, 3, 3, 4, 3)
p <- seq(0, 1, by = 0.01)
N <- 10

#### for loop ver
loglh <- lh <- NULL
for (i in 1:length(p)) {
  lh[i] <- prod(dbinom(y, prob = p[i], size = 10))
  loglh[i] <- sum(log(dbinom(y, prob = p[i], size = 10)))
}

#### sapply() ver
lh0 <- sapply(p, function(x) prod(dbinom(y, prob = x, size = 10)))

### Q2
df_lh <- tibble(p = p, lh = lh, loglh = loglh)
df_lh %>% 
  ggplot(aes(x = p,
             y = loglh)) +
  geom_line()

p_best <- df_lh %>% 
  arrange(desc(loglh)) %>% 
  slice(1) %>% 
  pull(p)

### Q3
N * p_best
mean(y)
 
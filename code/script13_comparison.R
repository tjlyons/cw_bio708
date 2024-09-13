#' ---
#' title: "Report week13 Model Comparison"
#' output: html_document
#' date: "2023-11-14"
#' author: Akira Terui
#' ---

#+ message = F, warning = F
rm(list = ls())
library(tidyverse)


# in class ----------------------------------------------------------------

set.seed(1)

# generate simulated data

## sample size
n <- 100

## parameters
b <- c(0.1, 0.5)

## hypothetical explanatory variable
x1 <- rnorm(n = n, mean = 0, sd = 1)

## design matrix
X <- model.matrix(~x1)

## matrix multiplication
## expected value
mu <- drop(X %*% b)

## add random noise
y <- rnorm(n = n, mean = mu, sd = 0.5)

## plot
df0 <- tibble(y = y, x1 = x1) 

df0 %>% 
  ggplot(aes(y = y,
             x = x1)) +
  geom_point()


# assess model performance

## R squared

## true model!
m1 <- lm(y ~ x1, data = df0)
summary(m1)


## add "useless" predictor
df0 <- df0 %>% 
  mutate(x2 = rnorm(n = n))

m2 <- lm(y ~ x1 + x2, df0)
summary(m2)

## LR test

logLik(m1)
logLik(m2)

anova(m1, m2, test = "Chisq")

m0 <- lm(y ~ 1, df0)

anova(m0, m1, test = "Chisq")

## AIC
AIC(m1)
AIC(m2)


# lab ---------------------------------------------------------------------

library(palmerpenguins)
df_pen0 <- penguins_raw

# Question 1
## approach 1
cnm <- colnames(penguins_raw) %>% 
  str_to_lower() %>% 
  str_replace_all(pattern = "\\s",
                  replace = "_") %>% 
  str_remove_all(pattern = "_\\(.{1,}\\)")

colnames(df_pen0) <- cnm

# ## approach 2
# df_pen0 <- penguins_raw %>%
#   rename_with(.fn = str_to_lower) %>%
#   rename_with(.fn = str_replace_all,
#               pattern = "\\s",
#               replace = "_") %>%
#   rename_with(.fn = str_remove_all,
#               pattern = "_\\(.{1,}\\)")
#  
# ## approach 3
# df_pen0 <- penguins_raw %>%
#   rename_with(.fn = function(x) {
#     str_to_lower(x) %>%
#       str_replace_all(pattern = "\\s",
#                       replace = "_") %>%
#       str_remove_all(pattern = "_\\(.{1,}\\)")
#   })

# Question 2

sp <- unique(df_pen0$species)

df_pen <- df_pen0 %>% 
  mutate(cc_binary = ifelse(clutch_completion == "Yes",
                            yes = 1,
                            no = 0),
         species = case_when(species == sp[1] ~ "adelie",
                             species == sp[2] ~ "gentoo",
                             species == sp[3] ~ "chinstrap")
         ) %>% 
  drop_na(culmen_length,
          culmen_depth,
          flipper_length,
          body_mass,
          sex)
  
# Analysis 1
m <- glm(cc_binary ~ species + 
           culmen_length + 
           culmen_depth + 
           flipper_length +
           body_mass +
           sex,
         data = df_pen,
         family = "binomial")

# model selection
# install.packages("MuMIn")
library(MuMIn)

options(na.action = "na.fail")
ms <- dredge(m, rank = "AIC")
subset(ms, delta < 2)

mgood <- get.models(ms, subset = delta < 2)
summary(mgood[[1]])

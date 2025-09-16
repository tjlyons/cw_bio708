library(tidyverse)

# central tendency measures -----------------------------------------------

# Q1

z <- exp(rnorm(n = 1000, mean = 0, sd = 0.1))
z_mu <- mean(z)
z_me <- median(z)
z_gmu <- prod(z)^(1/length(z))

# Q2 & Q3

hist_z <- tibble(x = z) %>% 
  ggplot(aes(x = z))+
  geom_histogram()+
  geom_vline(xintercept = z_mu, color = "blue")+
  geom_vline(xintercept = z_me, color = "red")+
  geom_vline(xintercept = z_gmu, color = "green")

# Q5

z_rev <- -z + max(z) + 0.1
zrev_mu <- mean(z_rev)
zrev_me <- median(z_rev)
zrev_gmu <- exp(mean(log(z_rev)))

hist_zrev <- tibble(x = z_rev) %>% 
  ggplot(aes(x = z_rev))+
  geom_histogram()+
  geom_vline(xintercept = zrev_mu, color = "blue")+
  geom_vline(xintercept = zrev_me, color = "red")+
  geom_vline(xintercept = zrev_gmu, color = "green")

## install.packages('patchwork')
library(patchwork)
hist_z / hist_zrev

# variation measures ------------------------------------------------------

w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w

# Q1

m <- w * 1000
print(m)

# Q2

w_sd <- sd(w)
print(w_sd)

m_sd <- sd(m)
print(m_sd)

w_mad <- median(abs(w - median(w)))
print(w_mad)

m_mad <- median(abs(m - median(m)))
print(m_mad)

# Q3

cv_w <- w_sd / mean(w)
print(cv_w)

cv_m <- m_sd / mean(m)
print(cv_m)

madmed_w <- w_mad / median(w)
print(madmed_w)

madmed_m <- m_mad / median(m)
print(madmed_m)


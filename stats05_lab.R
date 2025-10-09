pacman::p_load(tidyverse,
               patchwork,
               here)

df_pg <- PlantGrowth

df_anova <- df_pg %>% 
  ggplot(aes(x = group,
             y = weight)) +
  geom_violin(draw_quantiles = 0.5,
              alpha = 0.2) +
  geom_jitter(width = 0.1,
              alpha = 0.5)

m <- aov(weight ~ group,
    data = df_pg)

summary(m)

##install.packages("pwr")

pwr::pwr.anova.test(k = 3,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.8)

# pwr function affects on required sample size

#k (higher = fewer samples)
pwr::pwr.anova.test(k = 10,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.8)
pwr::pwr.anova.test(k = 2,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.8)

#f (higher = fewer samples)
pwr::pwr.anova.test(k = 3,
                    f = 0.1,
                    sig.level = 0.05,
                    power = 0.8)

pwr::pwr.anova.test(k = 3,
                    f = 0.8,
                    sig.level = 0.05,
                    power = 0.8)

#sig.level (higher = fewer samples)
pwr::pwr.anova.test(k = 3,
                    f = 0.5,
                    sig.level = 0.1,
                    power = 0.8)

#power (lower = fewer samples)
pwr::pwr.anova.test(k = 3,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.5)
pwr::pwr.anova.test(k = 3,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.2)
pwr::pwr.anova.test(k = 3,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 1)

#pwr function for calculating power

pwr::pwr.anova.test(k = 3,
                    n = 5,
                    f = 0.5,
                    sig.level = 0.05)

#k (lower = lower power)
pwr::pwr.anova.test(k = 2,
                    n = 5,
                    f = 0.5,
                    sig.level = 0.05)
pwr::pwr.anova.test(k = 5,
                    n = 5,
                    f = 0.5,
                    sig.level = 0.05)

#n (lower = lower power)
pwr::pwr.anova.test(k = 3,
                    n = 2,
                    f = 0.5,
                    sig.level = 0.05)
pwr::pwr.anova.test(k = 3,
                    n = 10,
                    f = 0.5,
                    sig.level = 0.05)

#f (lower = lower power)
pwr::pwr.anova.test(k = 3,
                    n = 5,
                    f = 0.1,
                    sig.level = 0.05)
pwr::pwr.anova.test(k = 3,
                    n = 5,
                    f = 0.9,
                    sig.level = 0.05)

#sig.level (lower = lower power)
pwr::pwr.anova.test(k = 3,
                    n = 5,
                    f = 0.5,
                    sig.level = 0.01)
pwr::pwr.anova.test(k = 3,
                    n = 5,
                    f = 0.5,
                    sig.level = 0.1)

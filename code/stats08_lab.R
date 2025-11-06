pacman::p_load(tidyverse,
               patchwork,
               here)
# GLM Lab

url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

print(df_fish)

m_fish <- glm(n_sp ~ distance + cat_area + hull_area,
              data = df_fish,
              family = "poisson")
summary(m_fish) ## distance = 0.0364*, cat_area = 0.7633, hull_area = 0.4496



m_cars <- glm(cbind(am, 1 - am) ~ mpg + hp + wt,
              data = mtcars,
              family = "binomial")
summary(m_cars) ## mpg = 0.6943, hp = 0.3079, wt = 0.0381*

## gaussian (normal) distribution yeilds bad p values
m_cars2 <- glm(am ~ mpg + hp + wt,
              data = mtcars,
              family = "gaussian")
summary(m_cars2) ## mpg = 0.1413, hp = 0.0093**, wt = 0.0107*


# effect size -------------------------------------------------------------
## scale() function standardizes values

df_fish <- df_fish %>% 
  mutate(std_dist = scale(distance),
         std_cat = scale(cat_area),
         std_hull = scale(hull_area)) 
 

m_fish_std <- glm(n_sp ~ std_dist + std_cat + std_hull,
              data = df_fish,
              family = "poisson")
summary(m_fish_std) ## std_dist = 0.0364*, std_cat = 0.7633, std_hull = 0.4496

coef(m_fish)
coef(m_fish_std)


# offset term -------------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_offset.csv"
df_offset <- read_csv(url)
print(df_offset)

x1 <- df_offset %>% 
  ggplot(aes(x = count,
             y = nitrate)) +
  geom_point(color = "blue")

x2 <- df_offset %>% 
  ggplot(aes(x = count,
             y = area)) +
  geom_point(color = "blue")

x3 <- df_offset %>% 
  ggplot(aes(x = count/area,
             y = nitrate)) +
  geom_point(color = "blue")

x1 + x2 + x3

## gaussian trial
df_offset <- df_offset %>% 
  mutate(density = count / area)

m_density <- glm(density ~ nitrate,
    data = df_offset,
    family = "poisson")
summary(m_density)

m_nitrate <- glm(count ~ nitrate,
              data = df_offset,
              family = "poisson")
summary(m_nitrate) ## p = 0.0215*

#incorporating an offset value
m_nitrate2 <- glm(count ~ nitrate + offset(log(area)),
                 data = df_offset,
                 family = "poisson")
summary(m_nitrate2) ## p = 2e-16***


# overdispersion ---------------------------------------------------------
## poisson doesn't always work right, type 1 error
## poisson assumes mu = var (lambda) causes some issues, mu = 2.62, var = 19.77

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_tadpole.csv"
df_tadpole <- read_csv(url)
print(df_tadpole)

g1 <- df_tadpole %>% 
  ggplot(aes(y = tadpole,
             x = permanence)) + 
           geom_point()

g2 <- df_tadpole %>% 
  ggplot(aes(y = tadpole,
             x = aqveg)) + 
  geom_point()

g1 + g2

glm(tadpole ~ permanence + aqveg,
    data = df_tadpole,
    family = "poisson") %>% 
  summary() ## permanence = 9.28e-08***, aqveg = 0.0354* (this is wrong)


mean(df_tadpole$tadpole)
var(df_tadpole$tadpole)

# negative binomial distribution
##install.packages("MASS")
MASS::glm.nb(tadpole ~ aqveg + permanence,
            data = df_tadpole) %>% 
  summary() ## permanence = 0.0146*, aqveg = 0.8089

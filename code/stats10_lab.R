pacman::p_load(tidyverse,
               patchwork,
               here,
               janitor,
               palmerspenguins)
## Model Comparison Lab

colnames(penguins_raw)

penguins_clean <- clean_names(penguins_raw)
colnames(penguins_clean)


## change input to clutch_completion
## - use ifelse()
## - combine with mutate()
unique(penguins_clean$clutch_completion)


penguins_clean <- penguins_clean %>% 
  mutate(clutch_completion = ifelse(clutch_completion == "Yes",
                                    yes = 1,
                                    no = 0))

unique(penguins_clean$clutch_completion)

## change species name input
## - use case_when()
## - combine with mutate()

sp <- unique(penguins_clean$species)

# simplify species names
penguins_clean <- penguins_clean %>% 
  mutate(species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adeliae",
                             species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo",
                             species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrap"))
unique(penguins_clean$species)

## remove NAs from the data frame

penguins_clean <- drop_na(penguins_clean,
                        culmen_length_mm,
                        culmen_depth_mm,
                        flipper_length_mm,
                        body_mass_g,
                        sex)

# Model selection

m_full <- glm(clutch_completion ~ species + 
      culmen_length_mm + 
      culmen_depth_mm + 
      flipper_length_mm + 
      body_mass_g,
    data = penguins_clean,
    family = "binomial")
summary(m_full)  

library(MuMIn)
options(na.action = "na.fail")
m_set <- dredge(m_full, rank = "AIC")
subset(m_set, delta < 2)
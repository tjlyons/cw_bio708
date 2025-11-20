pacman::p_load(tidyverse,
               patchwork,
               janitor,
               stringdist,
               here)
# Data cleaning


url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_messy.csv"
df_messy <- read_csv(url)


# check data entries before analysis --------------------------------------

## check data type for each column
## sapply & class

sapply(df_messy,
       FUN = class)

## check unique elements for each column
## sapply & unique

sapply(df_messy,
       FUN = unique)

## check possible type
## - stringdistmatrix()

collector <- unique(df_messy$collector)

stringdistmatrix(collector)

# text cleaning -----------------------------------------------------------

## remove white space
## str_squish()
a <- c(" a ", "a b", "b ", "a", "b")
str_squish(a)

## align text case
## str_to_lower() and str_to_upper()
b <- c("A", "a", "bB", "BB")
str_to_lower(b)
str_to_upper(b)

## replacing text 
## str_replace() and str_replace_all()
v <- c("a b", "a.b", "a b.c")
str_replace(v, "\\s", "_") %>%  ## \\s is RegEx for whitespace
  str_replace("\\.", "_") ## \\. is RegEx for period

str_replace(v, "\\s|\\.", "_") ## put | in between elements, "or"
## str_replace only replaces the first instance, this function is kinda pointless

str_replace_all(v, "\\s|\\.", "_") ## str_replace_all removes the second period

## removing text w/ str_remove_all()
x <- c("abc", "dd", "abd")
str_remove_all(x, "ab")

## extracting text 
str_extract(x, "ab")

## detect text
str_detect(x, "a") ## useful with ifelse()
ifelse(str_detect(x, "a"),
       yes = 1,
       no = 0)

y <- c("ab", "Ab")
str_detect(y, "^[Aa]") ## Begins with case-insensitive A

# date object -------------------------------------------------------------

d <- c("2024/06/01", "June 4 2024", "24.06.07")

lubridate::parse_date_time(d,
                           tz = "EST",
                           orders = c("Y/m/d", ## 4 digit year, months, date
                                      "B d Y", ## month name, day, 4 digit year\
                                      "y.m.d")) ## 2 digit year.month.date


# cleaning data -----------------------------------------------------------

print(df_messy)

df_messy %>% 
  mutate(collector = str_to_lower(collector)) %>% 
  mutate(species = str_to_lower(species) %>% 
           str_squish() %>% 
           str_replace_all("\\s|\\.", "_") %>% 
           str_remove("_$")) %>% 
  mutate(length_mm = str_replace(length_mm, ",", ".") %>% 
           str_remove("\\smm") %>% 
           as.numeric()) %>% 
  mutate(sample_date = lubridate::parse_date_time(sample_date,
                                                  tz = "EST",
                                                  orders = c("Y-m-d",
                                                             "B d Y",
                                                             "Y.m.d",
                                                             "d B Y")) %>% 
           as.Date()) %>% 
  mutate(recaptured = ifelse(str_detect(recaptured, "^[Yy]"),
                             yes = 1,
                             no = 0))

                             
         
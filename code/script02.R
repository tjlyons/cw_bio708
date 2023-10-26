#' ---
#' title: "Report week2"
#' output: html_document
#' date: "2023-08-22"
#' author: Akira Terui
#' ---

# vector ------------------------------------------------------------------

#ex.1a manually create a vector using c()
x <- c(1, 3, 4, 8)
x

#ex.1b character
x <- c("a", "b", "c")
x

#ex.1c logical
x <- c(TRUE, FALSE, FALSE)
x

#ex.2 sequence of numbers
x <- 1:5
x

#ex.3a replicate same numbers or characters
x <- rep(x = 2, times = 5) # replicate 2 five times
x

#ex.3b replicate same numbers or characters
x <- rep("a", 5) # replicate "a" five times
x

#ex.4a use seq() function
x <- seq(1, 5, by = 1)
x

#ex.4b use seq() function
x <- seq(1, 5, by = 0.1)
x

# check features ----------------------------------------------------------

x <- c(1.2, 3.1, 4.0, 8.2)
x

class(x)
typeof(x)
length(x)
sum(x)
mean(x)

y <- c("a", "b", "c")
class(y)
length(y)

# access ------------------------------------------------------------------

x <- c(2, 2, 3, 2, 5)
x[2] # access element #2

x[c(2,4)] # access elements #2 and 4

x[2:4] # access elements #2-4


# equation ----------------------------------------------------------------

# creating a vector
x <- c(2,2,3,2,5)

# ex.1a equal
x == 2

# ex.1b larger than
x > 2 

# ex.2a equal
x[x == 2]

# ex.2b larger than
x[x > 2]

# ex.3a equal
which(x == 2) # returns which elements are equal to 2

# ex.3b larger than
which(x > 2)


# matrix ------------------------------------------------------------------

#ex.1 cbind: combine objects by column
x <- cbind(c(1,2,3), c(4,5,6))
x

#ex.2 rbind: combine objects by row
x <- rbind(c(1,2,3), c(4,5,6))
x

#ex.3 matrix: specify elements and the number of rows (nrow) and columns (ncol)
x <- matrix(1:9, nrow = 3, ncol = 3)
x


# check features ----------------------------------------------------------

x <- matrix(1:9, nrow = 3, ncol = 3)
x

class(x)
typeof(x)
dim(x)


# access ------------------------------------------------------------------

x <- matrix(1:9, nrow = 3, ncol = 3)
x

x[2, 3] # access an element in row #2 and colum #3
x[2,] # access elements in row #2
x[c(2,3),] # access elements in rows #2 and 3
x[,c(2,3)] # access elements in columns #2 and 3

x == 2 # equal
x > 2

x[x == 2] # equal
x[x > 2] # larger than
which(x == 2, arr.ind = TRUE)
which(x > 2, arr.ind = TRUE)


# data frame --------------------------------------------------------------

# Create data frame
x <- c("Pristine", "Pristine", "Disturbed", "Disturbed", "Pristine") # Lake type
y <- c(1.2, 2.2, 10.9, 50.0, 3.0) # TSS: total suspended solids (mg/L)
df0 <- data.frame(LakeType = x, TSS = y) # x is named as "LakeType" while y is named as "TSS"
df0

colnames(df0) # call column names
df0$LakeType # access LakeType
df0$TSS # access TSS
df0[,1] # access column #1
df0[1,] # access row #1
df0[c(2,4),] # access row #2 and 4


# exercise ----------------------------------------------------------------

# vector
x1 <- c(1, 2, 3)
x2 <- rep(1, 6)
x3 <- seq(0, 10, length = 20)

x1 <- c("a", "b", "c")
x2 <- rep("a", 6)
x3 <- sample(letters, size = 20)

set.seed(1)
x <- rnorm(100)

which(x > 2)
x[which(x > 2)]

# matrix
m_x1 <- cbind(rep(1, 4), rep(2, 4), rep(3, 4), rep(3, 4))
m_x2 <- rbind(rep(1, 4), rep(2, 4), rep(3, 4), rep(3, 4))

m_xc1 <- cbind(rep("a", 4), rep("b", 4), rep("c", 4), rep("d", 4))
m_xc2 <- rbind(rep("a", 4), rep("b", 4), rep("c", 4), rep("d", 4))

set.seed(1)
m_x <- matrix(rnorm(100), nrow = 10, ncol = 10)
which(m_x > 2, arr.ind = TRUE)

mean(m_x[m_x > 2])

# data frame
df0 <- data.frame(x = sample(letters, size = 10),
                  y = rnorm(n = 10, mean = 0, sd = 1),
                  z = rnorm(n = 10, mean = 10, sd = 5))

class(df0$x)
class(df0$y)
class(df0$z)

set.seed(1)
x <- rnorm(100, mean = 10, sd = 3)
y <- rpois(100, lambda = 10)
z <- rep(c("VA", "NC"), 50)
df0 <- data.frame(temperature = x, abundance = y, state = z)

mean(df0[df0$state == "NC", "temperature"])
mean(df0[df0$state == "VA", "temperature"])

mean(df0[df0$state == "NC", "abundance"])
mean(df0[df0$state == "VA", "abundance"])


# tidyverse ---------------------------------------------------------------

#install.packages("tidyverse")
library(tidyverse)
iris <- as_tibble(iris)

# filter ------------------------------------------------------------------

# single match "=="
filter(iris, Species == "virginica")

# multiple match "%in%"
filter(iris, Species %in% c("virginica", "versicolor"))

# except "!="
filter(iris, Species != "virginica")

# except multiple "!(x %in% c("a", "b"))
filter(iris, !(Species %in% c("virginica", "versicolor")))

# greater than ">"
filter(iris, Sepal.Length > 5)

# equal & greater than ">="
filter(iris, Sepal.Length >= 5)

# less than "<"
filter(iris, Sepal.Length < 5)

# equal & less than "<="
filter(iris, Sepal.Length <= 5)


# arrange -----------------------------------------------------------------

# arrange in an ascending order
arrange(iris, Sepal.Length)

# arrange in an descending order
arrange(iris, desc(Sepal.Length))


# select ------------------------------------------------------------------

# select one column
select(iris, Sepal.Length)

# select multiple columns
select(iris, c(Sepal.Length, Sepal.Width))

# remove one column
select(iris, -Sepal.Length)

# remove multiple columns
select(iris, -c(Sepal.Length, Sepal.Width))

# select/remove multiple columns with a start rule
# starts_with("x")
select(iris, starts_with("Sepal"))
select(iris, -starts_with("Sepal"))

# select/remove multiple columns with an end rule
# ends_with("x")
select(iris, ends_with("Width"))
select(iris, -ends_with("Width"))

# mutate ------------------------------------------------------------------

# add a new column
x <- 1:150
mutate(iris, x = x)


# piping ------------------------------------------------------------------

# the following codes produce the same data frame
# apply functions separately
df_vir <- filter(iris, Species == "virginica")
df_vir_sl <- select(df_vir, Sepal.Length)
print(df_vir_sl)

# piping
iris %>% 
  filter(Species == "virginica") %>% 
  select(Sepal.Length)


# group operation ---------------------------------------------------------

# grouping by "Species", then take means "Speal.Length" for each species
iris %>% 
  group_by(Species) %>% 
  summarize(mu_sl = mean(Sepal.Length))

# grouping by "Species", then take means & SD "Speal.Length" for each species
iris %>% 
  group_by(Species) %>% 
  summarize(mu_sl = mean(Sepal.Length),
            sd_sl = sd(Sepal.Length))

# grouping by "Species", then take means "Speal.Length" for each species
iris %>% 
  group_by(Species) %>% 
  mutate(mu_sl = mean(Sepal.Length)) %>% 
  ungroup() %>% 
  view


# left_join ---------------------------------------------------------------

# matching by a single column
## left join by "Species": one to one
df1 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3))

df2 <- tibble(Species = c("A", "B", "C"),
              y = c(4, 5, 6))

left_join(x = df1,
          y = df2,
          by = "Species")


# exercise tidyverse ------------------------------------------------------

install.packages('ggplot2movies')
pacman::p_load(ggplot2movies,
               tidyverse)
data('movies')

# 1a
movies <- movies %>% 
  mutate(scl_rating = rating - mean(rating))

# 1b
movies_2000 <- movies %>% 
  filter(year >= 2000)

# 1c
movies_sel <- movies %>% 
  select(title, year, budget, length, rating, votes)

movies_sel <- movies %>% 
  select(title:votes)

movies_sel <- movies %>% 
  select(1:6)

# 1d
movies_rename <- movies %>% 
  rename(length_in_min = length)

# 2

movies %>% 
  group_by(year) %>% 
  summarize(mu_bu = mean(budget, na.rm = TRUE))

# 3
dat <- tibble(id = 1:10,
              x = rnorm(10),
              y = rnorm(10))

dat %>% 
  pivot_longer(cols = c(x, y),
               names_to = "var",
               values_to = "value")

# 4

movies %>% 
  filter(year >= 1990) %>% 
  select(title:votes, mpaa, Action, Drama) %>% 
  group_by(mpaa, Action) %>% 
  summarize(avg_rating = mean(rating))


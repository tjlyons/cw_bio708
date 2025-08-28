library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

# rows --------------------------------------------------------------------


## filter by value
filter(iris_sub, Species == "virginica")

## filter multiple values of one variable
## %in% including
filter(iris_sub, Species %in% c("virginica", "versicolor"))

## Except, exclude values by value
filter(iris_sub, Species != "virginica")

## Except, exclude multiple values of one variable
filter(iris_sub, !(Species %in% c("virginica", "versicolor")))

## filter values 
filter(iris_sub, Sepal.Length > 5)

# Multiple functions: Sepal.Length is less than 5 AND Species equals "setosa"
filter(iris_sub,Sepal.Length < 5 & Species == "setosa")

# Multiple functions: Either Sepal.Length is less than 5 OR Species equals "setosa"
filter(iris_sub, Sepal.Length < 5 | Species == "setosa")

## arranges data frame by increasing values of variable
arrange(iris_sub, Sepal.Length)

## arrange by descending value
arrange(iris_sub, desc(Sepal.Length))

## exercise
iris_3 <- filter(iris_sub, Sepal.Width > 3)

iris_setosa <- filter(iris_sub, Species == "setosa")

iris_3_setosa <- filter(iris_sub, Sepal.Width > 3 & Species == "setosa")

# columns -----------------------------------------------------------------


## select/isolate columns
select(iris_sub, Sepal.Length)

## select/isolate multiple columns
select(iris_sub, c(Sepal.Length, Sepal.Width))

## remove a column
select(iris_sub, -Sepal.Length)

## remove multiple columns
select(iris_sub, -c(Sepal.Length, Sepal.Width))

# select columns starting with "Sepal"
select(iris_sub, starts_with("Sepal"))

# remove columns starting with "Sepal"
select(iris_sub, -starts_with("Sepal"))

# select or remove columns ending with "Width"
select(iris_sub, ends_with("Width"))
select(iris_sub, -ends_with("Width"))

# nrow() returns the number of rows of the dataframe
(x_max <- nrow(iris_sub))

# create a vector from 1 to x_max
x <- 1:x_max

# add as a new column
# named `x` as `row_id` when added
mutate(iris_sub, row_id = x)

# twice `Sepal.Length` and add as a new column
mutate(iris_sub, sl_two_times = 2 * Sepal.Length)

## Exercise

iris_pw <- select(iris_sub, c(Petal.Width, Species))

iris_petal <- select(iris_sub, starts_with("Petal"))

mutate(iris_sub, row_id=x)
iris_pw_two <- mutate(iris_sub, pw_two_times = 2 * Petal.Width)

# piping ------------------------------------------------------------------

## ctrl+shift+m, allows sequential functions

## without piping
df_vir <- filter(iris_sub, Species == "virginica")
df_vir_sl <- select(df_vir, Sepal.Length)

print(df_vir_sl)

## with piping

df_vir_sl <- iris_sub %>%
  filter(Species == "virginica") %>% 
  select(Sepal.Length)

print(df_vir_sl)

## Exercise

iris_pipe <- iris_sub %>% 
  filter(Species == "setosa") %>% 
  mutate(pw_two_times = Petal.Width * 2)

print(iris_pipe)

# grouping ----------------------------------------------------------------

## ungrouped data set
print(iris_sub)

## grouping
iris_sub %>% 
  group_by(Species)

## summarizing
iris_sub %>% 
  group_by(Species) %>% 
  summarise(mu_sl = mean(Sepal.Length))

## summarizing multiple items
iris_sub %>% 
  group_by(Species) %>% 
  summarise(mu_sl = mean(Sepal.Length,),
            sum_sl = sum(Sepal.Width))

## summarizing as a mutation
iris_sub %>% 
  group_by(Species) %>% 
  mutate(mu_sl = mean(Sepal.Length)) %>% 
  ungroup()


# reshaping ---------------------------------------------------------------

## Wide data range
iris_w <- iris %>% 
  mutate(id = rep(1:50, 3)) %>% # add an ID column
  select(id, Sepal.Length, Species) %>% 
  pivot_wider(id_cols = "id", # unique row ID based on
              values_from = "Sepal.Length", # values in each cell from
              names_from = "Species") # new column names from

print(iris_w)

## Long data range
iris_l <- iris_w %>% 
  pivot_longer(cols = c("setosa",
                        "versicolor",
                        "virginica"), # columns with values to be reshaped
               names_to = "Species", # column IDs move to "Species"
               values_to = "Sepal.Length") # column values move to "Sepal.Length"

print(iris_l)


# joining -----------------------------------------------------------------

# matching by a single column
## left join by "Species": one to one
df1 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3))

df2 <- tibble(Species = c("A", "B", "C"),
              y = c(4, 5, 6))

left_join(x = df1,
          y = df2,
          by = "Species")

# matching by a single column
## left join by "Species": one to many
df3 <- tibble(Species = c("A", "A", "B", "C"),
              y = c(4, 5, 6, 7))

left_join(x = df1,
          y = df3,
          by = "Species")

# matching by a single column
## left join by "Species": one to missing
df4 <- tibble(Species = c("A", "A", "C"),
              y = c(4, 5, 7))

left_join(x = df1,
          y = df4,
          by = "Species")

# matching by multiple columns
## one to one
df5 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3),
              z = c("cool", "awesome", "magical"))

left_join(x = df1,
          y = df5,
          by = c("Species", "x"))

# matching by multiple columns
## one to many
df6 <- tibble(Species = c("A", "A", "B", "C"),
              x = c(1, 1, 2, 3),
              z = c("cool", "cool", "awesome", "magical"))

left_join(x = df1,
          y = df6,
          by = c("Species", "x"))

# matching by multiple columns
## one to missing
df6 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 4),
              z = c("cool", "awesome", "magical"))

left_join(x = df1,
          y = df6,
          by = c("Species", "x"))
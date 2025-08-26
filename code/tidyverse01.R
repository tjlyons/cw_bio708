library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

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

%>% 
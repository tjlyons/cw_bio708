#' ---
#' title: "Report week4"
#' output: html_document
#' date: "2023-09-05"
#' author: Akira Terui
#' ---

library(tidyverse)

# central tendency --------------------------------------------------------

# construct vectors x and y
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)

x
y

# arithmetic mean
## for vector x
n_x <- length(x) # the number of elements in x = the number of data points
sum_x <- sum(x) # summation for x
mu_x <- sum_x / n_x # arithmetic mean
print(mu_x) # print calculated value

## for vector y; we can calculate directly too
mu_y <- sum(y) / length(y)
print(mu_y) # print calculated value

## mean()
print(mean(x))
print(mean(y))

# geometric mean
## for vector x
prod_x <- prod(x) # product of vector x; x1 * x2 * x3...
n_x <- length(x)
mug_x <- prod_x^(1 / n_x) # ^ means power
print(mug_x)

## for vector y
mug_y <- prod(y)^(1 / length(y))
print(mug_y)

# median
## for vector x
x <- sort(x) # sort x from small to large
index <- (length(x) + 1) / 2 # (N + 1)/2 th index as length(x) is an odd number
med_x <- x[index]
print(med_x)

## for vector y
y <- sort(y) # sort y from small to large
med_y <- y[(length(y) + 1) / 2]
print(med_y)


# variation ---------------------------------------------------------------

# variance
## for x
sqd_x <- (x - mean(x))^2 # squared deviance
sum_sqd_x <- sum(sqd_x)
var_x <- sum_sqd_x / length(x)
print(var_x)

## for y
var_y <- sum((y - mean(y))^2) / length(y)
print(var_y)

## for x
sd_x <- sqrt(var_x) # sqrt(): square root
print(sd_x)

## for y
sd_y <- sqrt(var_y)
print(sd_y)

# coefficient of variation

## for x
cv_x <- sd_x / mean(x)
print(cv_x)

## for y
cv_y <- sd_y / mean(y)
print(cv_y)

# IQR

## for x
x_l <- quantile(x, 0.25) # quantile(): return quantile values, 25 percentile
x_h <- quantile(x, 0.75) # quantile(): return quantile values, 75 percentile
iqr_x <- abs(x_l - x_h) # abs(): absolute value
print(iqr_x)

# for y
y_q <- quantile(y, c(0.25, 0.75)) # return as a vector
iqr_y <- abs(y_q[1] - y_q[2]) # y_q[1] = 25 percentile; y_q[2] = 75 percentile
print(iqr_y)

# MAD

## for x
ad_x <- abs(x - median(x))
mad_x <- median(ad_x)
print(mad_x)

# for y
mad_y <- median(abs(y - median(y)))
print(mad_y)

# MAD/median

## for x
mm_x <- mad_x / median(x)
print(mm_x)

## for y
mm_y <- mad_y / median(y)
print(mm_y)

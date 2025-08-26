
# example code ------------------------------------------------------------



x <- c(1, 2)
x

y <- c(3, 4)
y

## produce 100 random numbers that follows a normal distribution
x <- rnorm(100, mean = 0, sd = 1)

## estimate mean
mean(x)

## estimate SD
sd(x)

## quick coding
z <- c(1, 2, 3)


# exercise ----------------------------------------------------------------

# create a vector with length 10, assign it to z
# z <- c(1, 2, 3 ...)
z <- 1:10
z <- seq(q,10, length=10)
z <- letters[1:10]

# create a matrix with 2 rows and 2 columns, assign it to m
m <- matrix(data = 1:4,nrow = 2, ncol = 2)
m <- cbind(c(1,2), c(3,4))
m <- rbind(c(1,2), c(3,4))

# data frame

df0 <- data.frame(name = c("Smith", "John", "Kate", "Akira"), height = c(154, 170, 156, 175))
df0

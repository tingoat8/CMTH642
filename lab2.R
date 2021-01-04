#Q1

set.seed(0)
data <- c(rnorm(75, mean = 36500, sd = 2000))
summary(data)
t.test(data, mu = 39000)

data <- c(65, 78, 88, 55, 48, 95, 66, 57, 79, 81)
mu <- mean(data)
sd <- sd(data)
t.test(data, mu = 75)

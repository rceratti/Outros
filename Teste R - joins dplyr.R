library(dplyr)

a <- data.frame(x = letters[1:5], y = rnorm(5))
b <- data.frame(x = letters[2:6], y = rpois(5, 10))

inner_join(a, b, by = 'x')
left_join(a, b, by = 'x')
semi_join(a, b, by = 'x')
anti_join(a, b, by = 'x')

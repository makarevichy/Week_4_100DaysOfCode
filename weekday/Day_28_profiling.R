library("profvis")
f <- function() {
  pause(0.1)
  g()
  h()
}
g <- function() {
  pause(0.1)
  h()
}
h <- function() {
  pause(0.1)
}

tmp <- tempfile()
Rprof(tmp, interval = 0.1)
f()
Rprof(NULL)

l <- profvis(f())
l

i <- function() {
  pause(0.1)
  10
}
j <- function(x) {
  x + 10
}
j(i())

mean1 <- function(x) mean(x)
mean2 <- function(x) sum(x) / length(x)
x <- runif(100)
stopifnot(all.equal(mean1(x), mean2(x)))
library(microbenchmark)
microbenchmark(
  mean1(x),
  mean2(x)
)

profvis({
  data(diamonds, package = "ggplot2")
  
  plot(price ~ carat, data = diamonds)
  m <- lm(price ~ carat, data = diamonds)
  abline(m, col = "red")
})

library(ggplot2)
p <- profvis({
  g <- ggplot(diamonds, aes(carat, price)) + geom_point(size = 1, alpha = 0.2)
  print(g)
})
p
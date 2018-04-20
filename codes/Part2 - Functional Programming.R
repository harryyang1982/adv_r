# 10. Functional Programming

## 10.1 Motivation

set.seed(1014)
df <- data.frame(replicate(6, sample(c(1:10, -99), 6, replace = TRUE)))
names(df) <- letters[1:6]
df

df$a[df$a == -99] <- NA
df$b[df$b == -99] <- NA
df$c[df$c == -99] <- NA
df$d[df$d == -99] <- NA
df$e[df$e == -99] <- NA
df$f[df$f == -99] <- NA

fix_missing <- function(x) {
  x[x == -99] <- NA
  x
}
df$a <- fix_missing(df$a)
df$b <- fix_missing(df$b)
df$c <- fix_missing(df$c)
df$d <- fix_missing(df$d)
df$e <- fix_missing(df$e)
df$f <- fix_missing(df$f)

# out <- vector("list", length(x))
# for (i in seq_along(x)) {
#   out[[i]] <- f(x[[i]], ...)
# }

fix_missing <- function(x) {
  x[x == -99] <- NA
  x
}
df[] <- lapply(df, fix_missing)

df[1:5] <- lapply(df[1:5], fix_missing)

# fix_missing_99 <- function(x) {
#   x[x == -99] <- NA
#   x
# }
# 
# fix_missing_999 <- function(x) {
#   x[x == -999] <- NA
#   x
# }
# fix_missing_9999 <- function(x) {
#   x[x == -999] <- NA
#   x
# }

missing_fixer <- function(na_value) {
  function(x) {
    x[x == na_value] <- NA
    x
  }
}
fix_missing_99 <- missing_fixer(-99)
fix_missing_999 <- missing_fixer(-999)

fix_missing_99(c(-99, -999))
fix_missing_999(c(-99, -999))

fix_missing <- function(x, na.value) {
  x[x == na.value] <- NA
  x
}

fix_missing(df, -99)

mean(df$a)
median(df$a)
sd(df$a)
mad(df$a)
IQR(df$a)

mean(df$b)
median(df$b)
sd(df$b)
mad(df$b)
IQR(df$b)

# initial edition of summary

summary <- function(x) {
  c(mean(x), median(x), sd(x), mad(x), IQR(x))
}
lapply(df, summary)

# advanced edition of summary function

summary <- function(x) {
  c(mean(x, na.rm = TRUE),
    median(x, na.rm = TRUE),
    sd(x, na.rm = TRUE),
    mad(x, na.rm = TRUE),
    IQR(x, na.rm = TRUE))
}
lapply(df, summary)

# better version of summary function

summary <- function(x) {
 funs <- c(mean, median, sd, mad, IQR)
 lapply(funs, function(f) f(x, na.rm = TRUE))
}
lapply(df, summary)

## 10.2 Anonymous functions

lapply(mtcars, function(x) length(unique(x)))
Filter(function(x) !is.numeric(x), mtcars)
integrate(function(x) sin(x) ^ 2, 0, pi)

formals(function(x = 4) g(x) + h(x))
body(function(x = 4) g(x) + h(x))
environment(function(x = 4) g(x) + h(x))

# This does not call the anonymous function
# (Note that "3" is not a valid function.)
# function(x) 3()

# With appropriate parenthesis, the function is called:
(function(x) 3)()

# So this anonymous function syntax
(function(x) x + 3)(10)

# behaves exactly the same as
f <- function(x) x + 3
f(10)

### 102.1 Exercises

#1. 
match.fun(mean)
#2. 
lapply(mtcars, function(x) sd(x) / mean(x))
#3. 

integrate(funs, lower, upper)
funs <- c(function(x) x ^ 2 - x, function(x) sin(x) + cos(x), function(x) exp(x) / x)
lower <- c(0, -pi, 10)
upper <- c(10, pi, 20)
df <- list(funs, lower, upper)
df

lapply(df, integrate(funs, lower, upper))

# integrate(function(x) x^2 - x, 0, 10)
# integrate(function(x) sin(x) + cos(x), -pi, pi)
# integrate(function(x) exp(x) / x, 10, 20)

#4.

## 10.3 Closure

power <- function(exponent) {
  function(x) {
    x ^ exponent
  }
}

square <- power(2)
square(2)
square(4)

cube <- power(3)
cube(2)
cube(4)

square
cube

environment(square)
environment(cube)
environment(power)
as.list(environment(square))
as.list(environment(cube))

library(pryr)
unenclose(square)
unenclose(cube)

power <- function(exponent) {
  print(environment())
  function(x) x ^ exponent
}
zero <- power(0)
environment(zero)

### 10.3.1 Function factories

### 10.3.2 Mutable state

new_counter <- function() {
  i <- 0
  function() {
    i <<- i + 1
    i
  }
}

counter_one <- new_counter()
counter_two <- new_counter()
counter_one()
counter_two()
counter_one()
counter_two()

# i <- 0
# new_counter2 <- function() {
#   i <<- i + 1
#   i
# }
# new_counter3 <- function() {
#   i <- 0
#   function() {
#     i <- i + 1
#     i
#   }
# }
# new_counter2()
# new_counter3()

### 10.3.3 Exercises

#2
bc <- function(lambda) {
  if (lambda == 0) {
    function(x) log(x)
  } else {
    function(x) (x ^ lambda - 1) / lambda
  }
}

#3
?approxfun

#4
?ecdf

#5
m1 <- moment(1)
m2 <- moment(2)

x <- runif(100)
stopifnot(all.equal(m1(x), 0))
stopifnot(all.equal(m2(x), var(x) * 99 / 100))

moment <- function(i){
  function(x) sum((x - mean(x)) ^ i) / length(x)
}

#6
pick <- function(i){
  function(x) x[[i]]
}

lapply(mtcars, pick(5))

## 10.4 List of functions

compute_mean <- list(
  base = function(x) mean(x),
  sum = function(x) sum(x) / length(x),
  manual = function(x) {
    total <- 0
    n <- length(x)
    for (i in seq_along(x)) {
      total <- total + x[i] / n
    }
    total
  }
)

x <- runif(1e5)
system.time(compute_mean$base(x))
system.time(compute_mean$sum(x))
system.time(compute_mean$manual(x))

lapply(compute_mean, function(f) f(x))

call_fun <- function(f, ...) f(...)
lapply(compute_mean, call_fun, x)

lapply(compute_mean, function(f) system.time(f(x)))
       
x <- 1:10
funs <- list(
  sum = sum,
  mean = mean,
  median = median
)
lapply(funs, function(f) f(x))

funs2 <- list(
  sum = function(x, ...) sum(x, ..., na.rm = TRUE),
  mean = function(x, ...) mean(x, ..., na.rm = TRUE),
  median = function(x, ...) median(x, ..., na.rm = TRUE)
)
lapply(funs2, function(f) f(x))

lapply(funs, function(f) f(x, na.rm = TRUE)) # better method

### 10.4.1 Moving lists of functions to the global environment

simple_tag <- function(tag) {
  force(tag)
  function(...) {
    paste0("<", tag, ">", paste0(...), "</", tag, ">")
  }
}
tags <- c("p", "b", "i")
html <- lapply(setNames(tags, tags), simple_tag)

html$p("This is ", html$b("bold"), " text.")

with(html, p("This is ", b("bold"), " text."))

attach(html)
p("This is ", b("bold"), " text.")
detach(html)

list2env(html, environment())
p("This is ", b("bold"), " text.")
rm(list = names(html), envir = environment())

## 10.5 Case study: numerical integration

midpoint  <-  function(f, a, b) {
  (b - a) * f((a + b) / 2)
}

trapezoid <- function(f, a, b) {
  (b - a) / 2 * (f(a) + f(b))
}

midpoint(sin, 0, pi)
trapezoid(sin, 0, pi)

midpoint_composite <- function(f, a, b, n = 10) {
  points <- seq(a, b, length.out = n + 1)
  h <- (b - a) / n
  
  area <- 0
  for (i in seq_len(n)) {
    area <- area + h * f((points[i] + points[i + 1]) / 2)
  }
  area
}

trapezoid_composite <- function(f, a, b, n = 10) {
  points <- seq(a, b, length.out = n + 1)
  h <- (b - a) / n
  
  area <- 0
  for (i in seq_len(n)) {
    area <- area + h / 2 * (f(points[i]) + f(points[i + 1]))
  }
  area
}

midpoint_composite(sin, 0, pi, n = 10)
midpoint_composite(sin, 0, pi, n = 100)
trapezoid_composite(sin, 0, pi, n = 10)
trapezoid_composite(sin, 0, pi, n = 100)

composite <- function(f, a, b, n = 10, rule) {
  points <- seq(a, b, length.out = n + 1)
  
  area <- 0
  for (i in seq_len(n)) {
    area <- area + rule(f, points[i], points[i + 1])
  }
  area
}
composite(sin, 0, pi, n = 10, rule = midpoint)
composite(sin, 0, pi, n = 10, rule = trapezoid)

simpson <- function(f, a, b) {
  (b - a) / 6 * (f(a) + 4 * f((a + b) / 2) + f(b))
}

boole <- function(f, a, b) {
  pos <- function(i) a + i * (b - a) / 4
  fi <- function(i) f(pos(i))
  
  (b - a) / 90 *
    (7 * fi(0) + 32 * fi(1) + 12 * fi(2) + 32 * fi(3) + 7 * fi(4))
}

composite(sin, 0, pi, n= 10, rule = simpson)
composite(sin, 0, pi, n = 10, rule = boole)

newton_cotes <- function(coef, open = FALSE) {
  n <- length(coef) + open

  function(f, a, b) {
    pos <- function(i) a + i * (b - a) / n
    points <- pos(seq.int(0, length(coef) - 1))
    
    (b - a) / sum(coef) * sum(f(points) * coef)
  }
}

boole <- newton_cotes(c(7, 32, 12, 32, 7))
milne <- newton_cotes(c(2, -1, 2), open = TRUE)
composite(sin, 0, pi, n = 10, rule = milne)
composite(sin, 0, pi, n = 10, rule = boole)

# 11 Functionals

randomise <- function(f) f(runif(1e3))
randomise(mean)
randomise(mean)
randomise(sum)

## 11.1 My first functional: lapply()

lapply2 <- function(x, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}

l <- replicate(20, runif(sample(1:10, 1)), simplify = TRUE)

# With a for loop
out <- vector("list", length(l))
for (i in seq_along(l)) {
  out[[i]] <- length(l[[i]])
}
unlist(out)

unlist(lapply(l, length))

# What class is each column?
unlist(lapply(mtcars, class))

# Divide each column by the mean
mtcars[] <- lapply(mtcars, function(x) x / mean(x))

# mtcars <- lapply(mtcars, function(x) x / mean(x)) # making list

trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)
unlist(lapply(trims, function(trim) mean(x, trim = trim)))

### 11.1.1 Looping patterns

xs <- runif(1e3)
res <- c()
for (x in xs) {
  # This is slow!
  res <- c(res, sqrt(x))
}

lapply(xs, function(x) {})
lapply(seq_along(xs), function(i) {})
lapply(names(xs), function(nm) {})

### 11.1.2 Exercises

#1.
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(100)

lapply(trims, function(trim) mean(x, trim = trim))
lapply(trims, mean, x= x)

#2.
scale01 <- function(x) {
  if(!is.numeric(x)) {
    print("it is not numeric")
    stop()
  } else {
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
  }
}

# lapply(mtcars, scale01)
# lapply(ggplot2::mpg, scale01)
lapply(ggplot2::mpg, scale01)
data.frame(lapply(iris, function(x) if (is.numeric(x)) scale01(x) else x))

lapply(ggplot2::mpg, function(x) if (is.numeric(x)) scale01(x) else x)

#3.

formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

lapply(formulas, lm, data = mtcars)

#4.

bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), replace = TRUE)
  mtcars[rows, ]
})

lapply(bootstraps, lm, formula = mpg ~ disp)
lapply(bootstraps, function(x) lm(formula = mpg ~ disp, data = x))

lf1 <- vector("list", length(bootstraps))
for (i in seq_along(bootstraps)) {
  lf1[[i]] <- lm(mpg ~ disp, data = bootstraps[[i]])
}
lf1

#5.

rsq <- function(mod) summary(mod)$r.squared
data.frame(lapply(lf1, rsq))

## 11.2 For loop functionals: friends of lapply()

sapply(mtcars, is.numeric)
vapply(mtcars, is.numeric, logical(1))
# ?vapply

sapply(list(), is.numeric)
vapply(list(), is.numeric, logical(1))

df <- data.frame(x = 1:10, y = letters[1:10])
sapply(df, class)
vapply(df, class, character(1))

df2 <- data.frame(x = 1:10, y = Sys.time() + 1:10)
sapply(df2, class)
vapply(df2, class, character(1))

sapply2 <- function(x, f, ...) {
  res <- lapply2(x, f, ...)
  simplify2array(res)
}

vapply2 <- function(x, f, f.value, ...) {
  out <- matrix(rep(f.value, length(x)), nrow = length(x))
  for (i in seq_along(x)) {
    res <- f(x[i], ...)
    stopifnot(
      length(res) == length(f.value),
      typeof(res) == typeof(f.value)
    )
    out[i, ] <- res
  }
  out
}

### 11.2.2 Multiple inputs: Map (and mapply)

# Generate some sample data

xs <- replicate(5, runif(10), simplify = FALSE)
ws <- replicate(5, rpois(10, 5) + 1, simplify = FALSE)

unlist(lapply(xs, mean))

unlist(lapply(seq_along(xs), function(i) {
  weighted.mean(xs[[i]], ws[[i]])
}))

unlist(Map(weighted.mean, xs, ws))

stopifnot(length(xs) == length(ws))
out <- vector("list", length(xs))
for (i in seq_along(xs)) {
  out[[i]] <- weighted.mean(xs[[i]], ws[[i]])
}

mtmeans <- lapply(mtcars, mean)
mtmeans[] <- Map('/', mtcars, mtmeans)
mtmeans

# In this case, equivalent to
mtcars[] <- lapply(mtcars, function(x) x / mean(x))
mtcars

unlist(Map(function(x, w) weighted.mean(x, w, na.rm = TRUE), xs, ws))

### 11.2.3 Rolling computations

rollmean <- function(x, n) {
  out <- rep(NA, length(x))
  offset <- trunc(n / 2)
  for (i in (offset + 1):(length(x) - n + offset + 1)) {
    out[i] <- mean(x[(i - offset):(i + offset - 1)])
  }
  out
}
x <- seq(1, 3, length = 1e2) + runif(1e2)
plot(x)
lines(rollmean(x, 5), col = "blue", lwd = 2)
lines(rollmean(x, 10), col = "red", lwd = 2)

x <- seq(1, 3, length.out = 1e2) + rt(1e2, df = 2) / 3
plot(x)
lines(rollmean(x, 5), col = "red", lwd = 2)

rollapply <- function(x, n, f, ...) {
  out <- rep(NA, length(x))
  
  offset <- trunc(n / 2)
  for (i in (offset + 1):(length(x) - n + offset + 1)) {
    out[i] <- f(x[(i - offset):(i + offset)], ...)
  }
  out
}
plot(x)
lines(rollapply(x, 5, median), col = "red", lwd = 2)

rollapply <- function(x, n, f, ...) {
  offset <- trunc(n / 2)
  locs <- (offset + 1):(length(x) - n + offset + 1)
  num <- vapply(
    locs,
    function(i) f(x[(i - offset):(i + offset)], ...),
    numeric(1)
  )
  c(rep(NA, offset), num)
}

plot(x)
lines(rollapply(x, 5, median), col = "red", lwd = 2)


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


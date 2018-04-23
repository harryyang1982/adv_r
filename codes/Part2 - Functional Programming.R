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

### 11.2.4 Parallelisation

lapply3 <- function(x, f, ...) {
  out <- vector("list", length(x))
  for (i in sample(seq_along(x))) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}
unlist(lapply(1:10, sqrt))
unlist(lapply3(1:10, sqrt))

library(parallel)
unlist(mclapply(1:10, sqrt, mc.cores = 4))

boot_df <- function(x) x[sample(nrow(x), replace = TRUE),]
rsquared <- function(mod) summary(mod)$r.square
boot_lm <- function(i) {
  rsquared(lm(mpg ~ wt + disp, data = boot_df(mtcars)))
}

system.time(lapply(1:500, boot_lm))
system.time(mclapply(1:500, boot_lm, mc.cores = 2))

### 11.2.5 Exercises

#1
vapply(mtcars, sd, numeric(1))
vapply(ggplot2::mpg[vapply(ggplot2::mpg, is.numeric, logical(1))], sd, numeric(1))

#2
sapply(ggplot2::mpg, class)

### example ###
a <- letters[1:3]
class(a) <- c("class1", "class2")
df <- data.frame(a = character(3))
df$a <- a
df$b <- a
class(sapply(df, class))

#3
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)
sapply(trials, "[[", "p.value")

#4
#5
lapply_nms <- function(X, FUN, ...){
  Map(FUN, X, names(X), ...)
}
lapply_nms(iris, function(x, y) c(class(x), y))

#6
testlist <- list(iris, mtcars, cars)
lapply(testlist, function(x) vapply(x, mean, numeric(1)))
testlist

#7

## 11.3 Manipulating matrices and data frames

## apply / tapply / plyr

### 11.3.1 Matrix and array operations

a <- matrix(1:20, nrow = 5)
apply(a, 1, mean)
apply(a, 2, mean)

a1 <- apply(a, 1, identity)
identical(a, a1)
identical(a, t(a1))

a2 <- apply(a, 2, identity)
identical(a, a2)

x <- matrix(rnorm(20, 0, 10), nrow = 4)
x1 <- sweep(x, 1, apply(x, 1, min), `-`)
x2 <- sweep(x1, 1, apply(x1, 1, max), `/`)
x
x1
x2

outer(1:3, 1:10, "*")

### 11.3.2 Group apply

pulse <- round(rnorm(22, 70, 10 / 3)) + rep(c(0, 5), c(10, 12))
group <- rep(c("A", "B"), c(10, 12))

tapply(pulse, group, length)
tapply(pulse, group, mean)

split(pulse, group)

tapply2 <- function(x, group, f, ..., simplify = TRUE) {
  pieces <- split(x, group)
  sapply(pieces, f, simplify = simplify)
}
tapply2(pulse, group, length)
tapply2(pulse, group, mean)

### 11.3.3 The plyr package

### 11.3.4 Exercises

#1. 

# for two dimensional cases everything is sorted by the other dimension
arr2 <- array(1:9, dim = c(3, 3), dimnames = list(paste0("row", 1:3),
                                                  paste0("col", 1:3)))
arr2
apply(arr2, 1, head, 1) # Margin is row
apply(arr2, 1, head, 9) # sorts by col

apply(arr2, 2, head, 1) # Margin is col
apply(arr2, 2, head, 9) # sorts by row

# 3 dimensional
arr3 <- array(1:27, dim = c(3,3,3), dimnames = list(paste0("row", 1:3),
                                                    paste0("col", 1:3),
                                                    paste0("time", 1:3)))
arr3
apply(arr3, 1, head, 1) # Margin is row
apply(arr3, 1, head, 27) # sorts by time and col

apply(arr3, 2, head, 1) # Margin is col
apply(arr3, 2, head, 27) # sorts by time and row

apply(arr3, 3, head, 1) # Margin is time
apply(arr3, 3, head, 27) # sorts by col and row

# 4 dimensional
arr4 <- array(1:81, dim = c(3,3,3,3), dimnames = list(paste0("row", 1:3),
                                                      paste0("col", 1:3),
                                                      paste0("time", 1:3),
                                                      paste0("var", 1:3)))
arr4

apply(arr4, 1, head, 1) # Margin is row
apply(arr4, 1, head, 81) # sorts by var, time, col

apply(arr4, 2, head, 1) # Margin is col
apply(arr4, 2, head, 81) # sorts by var, time, row

apply(arr4, 3, head, 1) # Margin is time
apply(arr4, 3, head, 81) # sorts by var, col, row

apply(arr4, 4, head, 1) # Margin is var
apply(arr4, 4, head, 81) # sorts by time, col, row

#2
v_tapply <- function(x, group, f, FUN.VALUE, ..., USE.NAMES = TRUE) {
  pieces <- split(x, group)
  vapply(pieces, f, FUN.VALUE, ..., USE.NAMES = TRUE)
}

#3
split2 <- function(x, f, drop = FALSE, ...){
  # there are three relevant cases for f. f is a character, f is a factor and all
  # levels occur, f is a factor and some levels don't occur.
  
  # first we check if f is a factor
  fact <- is.factor(f)
  
  # if drop it set to TRUE, we drop the non occuring levels.
  # (If f is a character, this has no effect.)
  if(drop){f <- f[, drop = TRUE]}
  
  # now we want all unique elements/levels of f
  levs <- if (fact) {unique(levels(f))} else {as.character(unique(f))}
  
  # we use these levels to subset x and supply names for the resulting output.
  setNames(lapply(levs, function(lv) x[f == lv, , drop = FALSE]), levs)
}

#4.

##11.4 Manipulating lists

###11.4.1 Reduce()
Reduce(`+`, 1:3) # -> ((1 + 2) + 3)
Reduce(sum, 1:3) # -> sum(sum(1, 2), 3)

Reduce2 <- function(f, x) {
  out <- x[[1]]
  for(i in seq(2, length(x))) {
    out <- f(out, x[[i]])
  }
  out
}

l <- replicate(5, sample(1:10, 15, replace = TRUE), simplify = FALSE)
str(l)

intersect(intersect(intersect(intersect(l[[1]], l[[2]]),
                              l[[3]]), l[[4]]), l[[5]])
Reduce(intersect, l)

### 11.4.2 Predicate functionals

where <- function(f, x) {
  vapply(x, f, logical(1))
}

df <- data.frame(x = 1:3, y = c("a", "b", "c"))
where(is.factor, df)
str(Filter(is.factor, df))

str(Find(is.factor, df))
Position(is.factor, df)

### 11.4.3 Exercises

#2
vapply_num <- function(x, fun, fun.value) {
  vapply(Filter(is.numeric, x), fun, fun.value)
}

#3
#4
Any <- function(l, pred){
  stopifnot(is.list(l))
  
  for (i in seq_along(l)){
    if (pred(l[[i]])) return(TRUE)
  }
  
  return(FALSE)
}
All <- function(l, pred){
  stopifnot(is.list(l))
  
  for (i in seq_along(l)){
    if (!pred(l[[i]])) return(FALSE)
  }
  
  return(TRUE)
}

#5
span_r <- function(l, pred){
  # We test if l is a list
  stopifnot(is.list(l))
  
  # we preallocate a logical vector and save the result
  # of the predicate function applied to each element of the list
  test <- vector("logical", length(l))
  for (i in seq_along(l)){
    test[i] <- (pred(l[[i]]))
  }
  # we return NA, if the output of pred is always FALSE
  if(!any(test)) return(NA_integer_)
  
  # Otherwise we look at the length encoding of TRUE and FALSE values.
  rle_test <- rle(test)
  # Since it might happen, that more than one maximum series of TRUE's appears,
  # we have to implement some logic, which might be easier, if we save the rle 
  # output in a data.frmame
  rle_test <- data.frame(lengths = rle_test[["lengths"]],
                         values = rle_test[["values"]],
                         cumsum = cumsum(rle_test[["lengths"]]))
  rle_test[["first_index"]] <- rle_test[["cumsum"]] - rle_test[["lengths"]] + 1
  # In the last line we calculated the first index in the original list for every encoding
  # In the next line we calculate a column, which gives the maximum 
  # encoding length among all encodings with the value TRUE
  rle_test[["max"]] <-  max(rle_test[rle_test[, "values"] == TRUE, ][,"lengths"])
  # Now we just have to subset for maximum length among all TRUE values and return the
  # according "first index":
  rle_test[rle_test$lengths == rle_test$max & rle_test$values == TRUE, ]$first_index
}

## 11.5 Mathematical functionals
integrate(sin, 0, pi)
str(uniroot(sin, pi * c(1 / 2, 3 / 2)))

str(optimize(sin, c(0, 2 * pi)))
str(optimize(sin, c(0, pi), maximum = TRUE))

poisson_nll <- function(x) {
  n <- length(x)
  sum_x <- sum(x)
  function(lambda) {
    n * lambda - sum_x * log(lambda)
  }
}

x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)
x2 <- c(6, 4, 7, 3, 3, 7, 5, 2, 2, 7, 5, 4, 12, 6, 9)
nll1 <- poisson_nll(x1)
nll2 <- poisson_nll(x2)

optimise(nll1, c(0, 100))$minimum
optimise(nll2, c(0, 100))$minimum

### 11.5.1 Exercises

arg_max <- function(x, f){
  x[f(x) == max(f(x))]
}
arg_max(-10:5, function(x) x^2)
arg_min <- function(x, f){
  x[f(x) == min(f(x))]
}
arg_min(-5:5, function(x) x^2)

## 11.6 Loops that should be left as is

trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) factor(x, levels = c("auto", "manual"))
)
for(var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}
mtcars

lapply(names(trans), function(var) {
  mtcars[[var]] <<- trans[[var]](mtcars[[var]])
})

### 11.6.2 Recusrive relationships

exps <- function(x, alpha) {
  s <- numeric(length(x) + 1)
  for (i in seq_along(s)) {
    if (i == 1) {
      s[i] <- x[i]
    } else {
      s[i] <- alpha * x[i - 1] + (1 - alpha) * s[i - 1]
    }
  }
  s
}
x <- runif(6)
exps(x, 0.5)

### 1.6.3 While loops

for ( i in 1:10) print(i)

i <- 1
while(i <= 10) {
  print(i)
  i <- i + 1
}

# i <- rgeom(0, 1)
i <- 0
while(TRUE) {
  if (runif(1) > 0.9) break
  i <- i + 1
}
i

## 11.7 A family of functions

add <- function(x, y) {
  stopifnot(length(x) == 1, length(y) == 1,
            is.numeric(x), is.numeric(y))
  x + y
}

rm_na <- function(x, y, identity) {
  if (is.na(x) && is.na(y)) {
    identity
  } else if (is.na(x)) {
    y
  } else {
    x
  }
}
rm_na(NA, 10, 0)
rm_na(10, NA, 0)
rm_na(NA, NA, 0)

add <- function(x, y, na.rm = FALSE) {
  if (na.rm && (is.na(x) || is.na(y))) rm_na(x, y, 0) else x + y
}
add(10, NA)
add(10, NA, na.rm = TRUE)
add(NA, NA)
add(NA, NA, na.rm = TRUE)

add(add(3, NA, na.rm = TRUE), NA, na.rm = TRUE)
add(3, add(NA, NA, na.rm = TRUE), na.rm = TRUE)

r_add <- function(xs, na.rm = TRUE) {
  Reduce(function(x, y) add(x, y, na.rm = na.rm), xs)
}
r_add(c(1, 4, 10))

r_add(NA, na.rm = TRUE)
r_add(numeric())

r_add <- function(xs, na.rm = TRUE) {
  Reduce(function(x, y) add(x, y, na.rm = na.rm), xs, init = 0)
}
r_add(c(1, 4, 10))
r_add(NA, na.rm = TRUE)
r_add(numeric())

v_add1 <- function(x, y, na.rm = FALSE) {
  stopifnot(length(x) == length(y), is.numeric(x), is.numeric(y))
  if (length(x) == 0) return(numeric())
  simplify2array(
    Map(function(x, y) add(x, y, na.rm = na.rm), x, y)
  )
}

v_add2 <- function(x, y, na.rm = FALSE) {
  stopifnot(length(x) == length(y), is.numeric(x), is.numeric(y))
  vapply(seq_along(x), function(i) add(x[i], y[i], na.rm = na.rm),
         numeric(1))
}

v_add1(1:10, 1:10)
v_add1(numeric(), numeric())

v_add1(c(1, NA), c(1, NA))
v_add1(c(1, NA), c(1, NA), na.rm = TRUE)

c_add <- function(xs, na.rm = FALSE) {
  Reduce(function(x, y) add(x, y, na.rm = na.rm), xs,
         accumulate = TRUE)
}
c_add(1:10)
c_add(10:1)

row_sum <- function(x, na.rm = FALSE) {
  apply(x, 1, add, na.rm = na.rm)
}
col_sum <- function(x, na.rm = FALSE) {
  apply(x, 2, add, na.rm = na.rm)
}
arr_sum <- function(x, dim, na.rm = FALSE) {
  apply(x, dim, add, na.rm = na.rm)
}

### 11.7.1 Exercises

#1
smaller_ <- function(x, y){
  if(anyNA(c(x, y))){return(NA_integer_)}
  out <- x
  if(y < x) {out <- y}
  out
}

larger_ <- function(x, y){
  if(anyNA(c(x, y))){return(NA_integer_)}
  out <- x
  if(y > x) {out <- y}
  out
}

rm_na <- function(x, y, identity) {
  if (is.na(x) && is.na(y)) {
    identity
  } else if (is.na(x)) {
    y
  } else {
    x
  }
}

smaller <- function(x, y, na.rm = FALSE) {
  stopifnot(length(x) == 1, length(y) == 1, is.numeric(x) | is.logical(x),
            is.numeric(y) | is.logical(y))
  if (na.rm && (is.na(x) || is.na(y))) rm_na(x, y, Inf) else smaller_(x,y)
}

larger <- function(x, y, na.rm = FALSE) {
  stopifnot(length(x) == 1, length(y) == 1, is.numeric(x) | is.logical(x),
            is.numeric(y) | is.logical(y))
  if (na.rm && (is.na(x) || is.na(y))) rm_na(x, y, -Inf) else larger_(x,y)
}

r_smaller <- function(xs, na.rm = TRUE) {
  Reduce(function(x, y) smaller(x, y, na.rm = na.rm), xs, init = Inf)
}
r_smaller(c(1:3, 4:(-1)))
r_smaller(NA, na.rm = TRUE)
r_smaller(numeric())

r_larger <- function(xs, na.rm = TRUE) {
  Reduce(function(x, y) larger(x, y, na.rm = na.rm), xs, init = -Inf)
}
r_larger(c(1:3), c(4:1))
r_larger(NA, na.rm = TRUE)
r_larger(numeric())

v_smaller1 <- function(x, y, na.rm = FALSE){
  stopifnot(length(x) == length(y), is.numeric(x) | is.logical(x), 
            is.numeric(y)| is.logical(x))
  if (length(x) == 0) return(numeric())
  simplify2array(
    Map(function(x, y) smaller(x, y, na.rm = na.rm), x, y)
  )
}

v_smaller2 <- function(x, y, na.rm = FALSE) {
  stopifnot(length(x) == length(y), is.numeric(x) | is.logical(x), 
            is.numeric(y)| is.logical(x))
  vapply(seq_along(x), function(i) smaller(x[i], y[i], na.rm = na.rm),
         numeric(1))
}

v_smaller1(1:10, c(2,1,4,3,6,5,8,7,10,9))
#>  [1] 1 1 3 3 5 5 7 7 9 9
v_smaller2(1:10, c(2,1,4,3,6,5,8,7,10,9))
#>  [1] 1 1 3 3 5 5 7 7 9 9

v_smaller1(numeric(), numeric())
#> numeric(0)
v_smaller2(numeric(), numeric())
#> numeric(0)

v_smaller1(c(1, NA), c(1, NA), na.rm = FALSE)
#> [1]  1 NA
v_smaller2(c(1, NA), c(1, NA), na.rm = FALSE)
#> [1]  1 NA

v_smaller1(NA,NA)
#> [1] NA
v_smaller2(NA,NA)
#> [1] NA

row_min <- function(x, na.rm = FALSE) {
  apply(x, 1, r_smaller, na.rm = na.rm)
}
col_min <- function(x, na.rm = FALSE) {
  apply(x, 2, r_smaller, na.rm = na.rm)
}
arr_min <- function(x, dim, na.rm = FALSE) {
  apply(x, dim, r_smaller, na.rm = na.rm)
}

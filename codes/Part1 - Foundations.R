#2 Data Structures

## 2.1 Vectors

### 2.1.1 Atomic Vectors

dbl_var <- c(1, 2.5, 4.5)
int_var <- c(1L, 6L, 10L)
log_var <- c(TRUE, FALSE, T, F)
chr_var <- c("these are", "some strings")

c(1, c(2, c(3, 4)))
c(1, 2, 3, 4)

#### 2.1.1.1 Types and tests

int_var <- c(1L, 6L, 10L)
typeof(int_var)
is.integer(int_var)
is.atomic(int_var)

dbl_var <- c(1, 2.5, 4.5)
typeof(dbl_var)
is.double(dbl_var)
is.atomic(dbl_var)

is.numeric(int_var)
is.numeric(dbl_var)

#### 2.1.1.2 Coercion

str(c("a", 1))

x <- c(FALSE, FALSE, TRUE)
as.numeric(x)
sum(x)
mean(x)

### 2.1.2 Lists

x <- list(1:3, "a", c(TRUE, FALSE, TRUE), c(2.3, 5.9))
str(x)

x <- list(list(list(list())))
str(x)
is.recursive(x)
x <- list(list(1, 2), c(3,4))
y <- c(list(1, 2), c(3, 4))
str(x)
str(y)

is.list(mtcars)
mod <- lm(mpg ~ wt, data = mtcars)
is.list(mod)

### 2.1.3 Exercises

c(1, FALSE)
c("a", 1)
c(list(1), "a")
c(TRUE, 1L)

## 2.2 Attributes

y <- 1:10
attr(y, "my_attribute") <- "This is a vector"
attr(y, "my_attribute")
str(attributes(y))

structure(1:10, my_attribute = "This is a vector")

######## most attributes are lost when modifying a vector
attributes(y[1])
attributes(sum(y))
attributes(y)

#### 2.2.0.1 Names

x <- c(a = 1, b = 2, c = 3)
attributes(x)
x <- 1:3; names(x) <- c("a", "b", "c")
x
attributes(x)
x <- setNames(1:3, c("a", "b", "c"))
attributes(x)

y <- c(a = 1, 2, 3)
names(y)

z <- c(1, 2, 3)
names(z)

unname(x)
# names(x) <- NULL

### 2.2.1 Factors

x <- factor(c("a", "b", "b", "a"))
x
class(x)
typeof(x)
levels(x)

x[2] <- "c"
x
c(factor("a"), factor("b"))

sex_char <- c("m", "m", "m")
sex_factor <- factor(sex_char, levels = c("m", "f"))
table(sex_char)
table(sex_factor)

# Reading in "text" instead of from a file here:
z <- read.csv(text = "value\n12\n1\n.\n9")
typeof(z$value)
as.double(z$value)
class(z$value)
as.double(as.character(z$value))
z <- read.csv(text = "value\n12\n1\n.\n9", na.strings = ".")
typeof(z$value)
class(z$value)
z$value

### 2.2.2 Exercises

k <- structure(1:5, comment = "my attribute")
attributes(k)

f1 <- factor(letters)
str(f1)
levels(f1) <- rev(levels(f1))
str(f1)

f2 <- rev(factor(letters))
f3 <- factor(letters, levels = rev(letters))

f1
f2
f3

## 2.3 Matrices and arrays

a <- matrix(1:6, ncol = 3, nrow = 2)
b <- array(1:12, c(2, 3, 2))
c <- 1:6
dim(c) <- c(3, 2)
c
dim(c) <- c(2, 3)
c
length(a)
nrow(a)
ncol(a)
rownames(a) <- c("A", "B")
colnames(a) <- c("a", "b", "c")
a

length(b)
dim(b)
dimnames(b) <- list(c("one", "two"), c("a", "b", "c"), c("A", "B"))
b

str(1:3)
str(matrix(1:3, ncol = 1))
str(matrix(1:3, nrow = 1))
str(array(1:3, 3))

l <- list(1:3, "a", TRUE, 1.0)
dim(l) <- c(2, 2)
l

### 2.3.1 Exercises

dim(z)
is.array(a)
is.array(c)
dim(c)

x1 <- array(1:5, c(1, 1, 5))
x2 <- array(1:5, c(1, 5, 1))
x3 <- array(1:5, c(5, 1, 1))

## 2.4 Data Frames

length(mtcars)
ncol(mtcars)
nrow(mtcars)

### 2.4.1 Creation

df <- data.frame(x = 1:3, y = c("a", "b", "c"))
str(df)

df <- data.frame(x = 1:3, y = c("a", "b", "c"),
                 stringsAsFactors = FALSE)
df
str(df)

### 2.4.2 Testing and coercion

typeof(df)
class(df)
is.data.frame(df)

### 2.4.3 Combining data frames

cbind(df, data.frame(z = 3:1))
rbind(df, data.frame(x = 10, y = "z"))

bad <- data.frame(cbind(a = 1:2, b = c("a", "b")))
str(bad)

good <- data.frame(a = 1:2, b = c("a", "b"),
                   stringsAsFactors = FALSE)
str(good)

### 2.4.4 Special columns

df <- data.frame(x = 1:3)
df$y <- list(1:2, 1:3, 1:4)
df

data.frame(x = 1:3, y = list(1:2, 1:3, 1:4))

dfl <- data.frame(x = 1:3, y = I(list(1:2, 1:3, 1:4)))
dfl
str(dfl)
str(df)
dfl[2, "y"]
df$y

dfm <- data.frame(x = 1:3, y = I(matrix(1:9, nrow = 3)))
str(dfm)
dfm
dfm[2, "y"]

#3 Subsetting

## 3.1 Data Types

### 3.1.1 Atomic vectors

x <- c(2.1, 4.2, 3.3, 5.4)

# Positive integers return elements at the specified positions:
x[c(3, 1)]
x[order(x)]

x[c(1, 1)]
x[c(2.1, 2.9)]

# Negative integers omit elements at the specified positions:
x[-c(3, 1)]
# x[c(-1, 2)] # can't mix positive and negative integers in a single subset:

# Logical vectors select elements where the corresponding logical value is TRUE.

x[c(TRUE, TRUE, FALSE, FALSE)]
x[x > 3]
x[c(TRUE, FALSE)]
x[c(TRUE, FALSE, TRUE, FALSE)] # as same as above
x[c(TRUE, TRUE, NA, FALSE)]

# Nothing returns the original vector.
x[]

# Zero returns a zero-length vector
x[0]

# Character vectors to return elements with matching names
(y <- setNames(x, letters[1:4]))
y[c("d", "c", "a")]
y[c("a", "a", "a")]
z <- c(abc = 1, def = 2)
z[c("a", "d")] # When subsetting with [ names are always matched exactly

### 3.1.2 Lists

### 3.1.3 Matrices and arrays

a <- matrix(1:9, nrow = 3)
colnames(a) <- c("A", "B", "C")
a[1:2, ]
a[c(T, F, T), c("B", "A")]
a[0, -2]
# a[1, 0]

(vals <- outer(1:5, 1:5, FUN = "paste", sep = ","))
vals[c(4, 15)] # retrieve 4th element and 15th element in the entire matrix => "4,1", "5, 3"

vals <- outer(1:5, 1:5, FUN = "paste", sep = ",")
select <- matrix(ncol = 2, byrow = TRUE, c(
  1, 1,
  3, 1,
  2, 4
))
vals[select] # subsetting with matrix as positions like row x by column y (x, y)

### 3.1.4 Data Frames

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df[df$x == 2, ]
df[c(1, 3), ]
df[c("x", "z")] # like a list
df[, c("x", "z")] # like a matrix

str(df["x"]) # list subsetting returns the data.frame
str(df[, "x"]) # matrix subsetting simplifies by default

### 3.1.5 S3 objects

### 3.1.6 S4 objects

### 3.1.7 Exercises
#### 3.1.7.1
mtcars[mtcars$cyl == 4, ]
mtcars[-c(1:4), ]
mtcars[mtcars$cyl <= 5,]
mtcars[mtcars$cyl == 4| mtcars$cyl == 6,]
mtcars[mtcars$cyl %in% c(4, 6),]
identical(mtcars[mtcars$cyl == 4| mtcars$cyl == 6,], mtcars[mtcars$cyl %in% c(4, 6),])

#### 3.1.7.2
x <- 1:5
x[NA]
x[NA_real_]

#### 3.1.7.3
x <- outer(1:5, 1:5, FUN = "*")
x[upper.tri(x)]

#### 3.1.7.4
# mtcars[1:20]
# mtcars[1:10]
mtcars[1:20, ] # difference is from columns and rows. the above is subsetting columns and the below is subsetting rows

#### 3.1.7.5
diag(x)

diag_own <- function(x) {
  matrix_make = 1:nrow(x)
  matrix_make ^ 2
}
diag_own(x)

#### 3.1.7.6
df[is.na(df)] <- 0
str(df)

## 3.2 Subsetting operators

a <- list(a = 1, b = 2)
a[[1]]
a[["a"]]

b <- list(a = list(b = list(c = list(d = 1))))
b[[c("a", "b", "c", "d")]]
b[["a"]][["b"]][["c"]][["d"]]

mtcars[[1]]
mtcars[["cyl"]]

### 3.2.1 Simplifying vs. preserving subsetting

# Atomic vector: removes names
x <- c(a = 1, b = 2)
x[1]
x[[1]]

# List: return the object inside the list, not a single element list
y <- list(a = 1, b = 2)
str(y[1])
str(y[[1]])

# Factor: drops any unused levels
z <- factor(c("a", "b"))
z[1]
z[1, drop = TRUE]

# Matrix or array: if any of the dimensions has length 1, drops that dimension
a <- matrix(1:4, nrow = 2)
a[1, , drop = FALSE]
a[1,]

# Data frame: if output is a single column, returns a vector instead of a data frame
df <- data.frame(a = 1:2, b = 1:2)
str(df[1])
str(df[[1]])
str(df[, "a", drop = FALSE])
str(df[, "a"])

### 3.2.2 $

var <- "cyl"
# mtcars$var
mtcars[[var]]

x <- list(abc = 1)
x$a
x[["a"]]

### 3.2.3 Missing/out of bounds indices

x <- 1:4
str(x[5])
str(x[NA_real_])
str(x[NULL])

### 3.2.4 Exercises

mod <- lm(mpg ~ wt, data = mtcars)
str(mod)
mod$df.residual
summary(mod)$r.squared

## 3.3 Subsetting and assignment

x <- 1:5
x[c(1, 2)] <- 2:3
x

x[-1] <- 4:1
x

x[c(1, 1)] <- 3
x

# x[c(1, NA)] <- c(1, 2) # NAs are not allowed in subscripted assignments
x[c(T, F, NA)] <- 1
x

df <- data.frame(a = c(1, 10, NA))
df$a[df$a < 5] <- 0
df$a

mtcars[] <- lapply(mtcars, as.integer)
mtcars
mtcars <- lapply(mtcars, as.integer)
mtcars

x <- list(a = 1, b = 2)
x[["b"]] <- NULL
str(x)

y <- list(a = 1)
y["b"] <- list(NULL)
str(y)

## 3.4 Applications

### 3.4.1 Lookup tables (character subsetting)

x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)
lookup[x]
unname(lookup[x])

unname(c(m = "Known", f = "Known", u = "Unknown")[x])

### 3.4.2 Matching and merging by hand (integer subsetting)

grades <- c(1, 2, 2, 3, 1)
info <- data.frame(
  grade = 3:1,
  desc = c("Excellent", "Good", "Poor"),
  fail = c(F, F, T)
)

# Using match
id <- match(grades, info$grade)
info[id, ]

# Using rownames
rownames(info) <- info$grade
info[as.character(grades), ]

### 3.4.3 Random samples/bootstrap (integer subsetting)

df <- data.frame(x = rep(1:3, each = 2), y = 6:1, z = letters[1:6])
set.seed(10)
df[sample(nrow(df)), ]
df[sample(nrow(df), 3), ]
df[sample(nrow(df), 6, replace = T), ]

### 3.4.4 Ordering (integer subsetting)

x <- c("b", "c", "a")
order(x)
x[order(x)]

df2 <- df[sample(nrow(df)), 3:1]
df2

df2[order(df2$x), ]
df2[, order(names(df2))]

### 3.4.5 Expanding aggregated counts (integer subsetting)

df <- data.frame(x = c(2, 4, 1), y = c(9, 11, 6), n = c(3, 5, 1))
rep(1:nrow(df), df$n)
df[rep(1:nrow(df), df$n), ]

### 3.4.6 Removing columns from data frames (character subsetting)

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df$z <- NULL

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df[c("x", "y")]

df[setdiff(names(df), "z")]

### 3.4.7 Selecting rows based on a condition (logical subsetting)

mtcars[mtcars$gear == 5, ]
mtcars[mtcars$gear == 5 & mtcars$cyl == 4, ]

subset(mtcars, gear == 5)
subset(mtcars, gear == 5 & cyl == 4)

### 3.4.8 Boolean algebra vs. sets (logical & integer subsetting)

x <- sample(10) < 4
which(x)

unwhich <- function(x, n) {
  out <- rep_len(FALSE, n)
  out[x] <- TRUE
  out
}
unwhich(which(x), 10)

(x1 <- 1:10 %% 2 == 0)
(x2 <- which(x1))
x1
x2
(y1 <- 1:10 %% 5 == 0)
(y2 <- which(y1))

# X & Y <-> intersect(x, y)
x1 & y1
intersect(x2, y2)

x1 | y1
union(x2, y2)

x1 & !y1
setdiff(x2, y2)

# xor(X, Y) <-> setdiff(union(x, y), intersect(x, y))
xor(x1, y1)
setdiff(union(x2, y2), intersect(x2, y2))

### 3.4.9 Exercises

# 4. Vocabularies

## 4.1 The basics

str
all.equal
?ceiling
?sign
?trunc
round(.5 + -2:4)
x1 <- seq(-2, 4, by = .5)
x1[trunc(x1) != floor(x1)]
?signif
signif(x1)
?prod
prod(x1)
prod(1:7)
?missing
?on.exit
?xor
?diag
?sweep
?data.matrix
?rep_len
?seq_len
?rev
?choose
choose(36, 2)

?lapply
?vapply

## 4.2 Common data structures

?ISOdate
?ISOdatetime
?strftime
?strptime
?difftime
?julian

?grep
?agrepl
?chartr
?nchar
?substr
?nlevels
?findInterval
?interaction
options(stringAsFactors = F)

?array
?dimnames
?aperm

## 4.3 Statistics

# Ordering and tabulating
?duplicated
?rank
?ftable

# Linear Models
?fitted
?resid
?rstandard
?hat
?df
?logLik
?I
?confint
?vcov
?contrasts

# Miscellaneous tests
?apropos("\\.test$")

# Random variables
# (q, p, d, r) * (beta, binom, cauchy, chisq, exp, f, gamma, geom, hyper, lnorm, logis, multinom, nbinom, norm, pois, signrank, t, unif, weibull, wilcox, birthday, tukey)

# Matrix algebra
?crossprod
?tcrossprod
?eigen
?qr
?svd
# %*% %o% outer
?rcond
?solve

## 4.4 Working with R

# Workspace
?ls
?exists
?rm
?getwd
?setwd
?q
?source
?require

# Help
?help.search
?apropos
?RSiteSearch
?citation
?demo

# Debugging
?traceback
?browser
?recover
?tryCatch
?try

## 4.5 I/O

# output
?print
?cat
?message
?dput
?sink
?format
?capture.output

# Reading and writing data
?data
?count.fields
?read.delim
?write.delim
?read.fwf
?readLines
?writeLines
?readRDS
?saveRDS
?save

# Files and directories
?dir
?basename
?dirname
?tools::file_ext
?file.path
?path.expand
?normalizePath
?file.choose
?file.copy
?file.create
?file.remove
?file.rename
?dir.create
?file.exists
?file.info
?tempdir
?tempfile
?download.file

#5 Style guide

## 5.1 Notation and naming
### 5.1.1 File names

# Good
# fit-models.R
# utility-functions.R

## 5.1.2 Object names

# Good
# day_one
# day_one

# Bad
# first_day_of_the_month
# DayOne
# dayone
# djm1

## 5.2 Syntax
### 5.2.1 Spacing

# Good
# average <- mean(feet / 12 + inches, na.rm = T)
# Bad
# average<-mean(feet/12+inches,na.rm=T)

# Good
# if (debug) do(x)
# plot(x, y)

# Bad
# if(debug)do(x)
# plot (x, y)

### 5.2.2 Curly braces

# Good
# if (y < 0 && debug) {
#   message("Y is negative")
# }

### 5.2.3 Line length

### 5.2.4 Indentation

# long_function_name <- function(a = "a long argument",
#                                b = "another argument",
#                                c = "another long argument") {
#   # As usual code is indented by two spaces.
# }

### 5.2.5 Assignment

#6 Functions

### quiz

x <- 10
f1 <- function(x) {
  function() {
    x + 10
  }
}
f1(1)()

`+`(1, `*`(2, 3))
1 + 2 * 3

# mean(, TRUE, x = c(1:10, NA))
mean(c(1:10, NA), na.rm = F)

f2 <- function(a, b) {
  a * 10
}
f2(10, stop("This is an error!"))

## 6.1 Function components

f <- function(x) x^2
f

formals(f)
body(f)
environment(f)

### 6.1.1 Primitive functions

sum
formals(sum)
body(sum)
environment(sum)

### 6.1.2 Exercises

objs <- mget(ls("package:base"), inherits = TRUE)
funs <- Filter(is.function, objs)

## 6.2 Lexical scoping

x <- 10
x

### 6.2.1 Naming masking

f <- function() {
  x <- 1
  y <- 2
  c(x, y)
}
f()
rm(f)

# If a name isn't defined inside a function, R will loook one level up.

x <- 2
g <- function() {
  y <- 1
  c(x, y)
}
g()
rm(x, g)

x <- 1
h <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}
h()

j <- function(x) {
  y <- 2
  function() {
    c(x, y)
  }
}
k <- j(1)
k()
rm(j, k)

### 6.2.2 Functions vs. variables

l <- function(x) x + 1
m <- function() {
  l <- function(x) x * 2
  l(10)
}
m()
rm(l, m)

n <- function(x) x / 2
o <- function() {
  n <- 10
  n(n)
}
o()
rm(n, o)

### 6.2.3 A fresh start

j <- function() {
  if (!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  print(a)
}
j()

rm(j)

### 6.2.4 Dynamic lookup

f <- function() x
x <- 15
f()

x <- 20
f()

f <- function() x + 1
codetools::findGlobals(f)

environment(f) <- emptyenv()
f()

`(` <- function(e1) {
  if (is.numeric(e1) && runif(1) < 0.1) {
    e1 + 1
  } else {
    e1
  }
}
replicate(50, (1 + 2))
rm("(")

### 6.2.5 Exercises

c <- 10
c(c = c)

f <- function(x) {
  f <- function(x) {
    f <- function(x) {
      x ^ 2
    }
    f(x) + 1
  } 
  f(x) * 2
}
f(10)

## 6.3 Every operation is a function call

x <- 10; y <- 5
x + y
`+`(x, y)

for(i in 1:2) print(i)
`for`(i, 1:2, print(i))
if (i == 1) print("yes!") else print("no.")
`if`(i == 1, print("yes!"), print("no."))

x[3]
`[`(x, 3)

{ print(1); print(2); print(3) }
`{`(print(1), print(2), print(3))

add <- function(x, y) x + y
sapply(1:10, add, 3)

sapply(1:5, `+`, 3)
sapply(1:5, "+", 3)

sapply(1:10, `+`, 3)

?sapply

x <- list(1:3, 4:9, 10:12)
sapply(x, "[", 2)
sapply(x, function(x) x[2])

## 6.4 Function arguments

### 6.4.1 Calling functions

f <- function(abcdef, bcde1, bcde2) {
  list(a = abcdef, b1 = bcde1, b2 = bcde2)
}

str(f(1, 2, 3))
str(f(2, 3, abcdef = 1))
str(f(2, 3, a = 1))
str(f(1, 3, b = 1))

mean(1:10)
mean(1:10, trim = 0.05)

# overkill
mean(x = 1:10)

# confusing
mean(1:10, n = T)
# mean(1:10, , FALSE)
mean(1:10, 0.05)
# mean(, TRUE, x = c(1:10, NA))

### 6.4.2 Calling a function given a list of arguments

args <- list(1:10, na.rm = TRUE)

do.call(mean, list(1:10, na.rm = TRUE))
do.call(mean, args)
mean(1:10, na.rm = TRUE)

### 6.4.3 Default and missing arguments

f <- function(a = 1, b = 2) {
  c(a, b)
}
f()

g <- function(a = 1, b = a * 2) {
  c(a, b)
}
g()
g()
g(10)

h <- function(a = 1, b = d) {
  d <- (a + 1) ^ 2
  c(a, b)
}
h()
h(10)

i <- function(a, b) {
  c(missing(a), missing(b))
}
i()
i(a = 1)
i(b = 2)
i(1, 2)
i(1)

### 6.4.4 Lazy evaluation

f <- function(x) {
  10
}
f(stop("This is an error!"))

# f <- function(x) {
#   force(x)
#   10
# }
# f(stop("This is an error!"))
# f()

add <- function(x) {
  function(y) x + y
}
adders <- lapply(1:10, add)
adders[[1]](10)
adders[[10]](10)

adders[[1]](1)
# adders[1](1)

add <- function(x) {
  force(x)
  function(y) x + y
}
adders2 <- lapply(1:10, add)
adders2
adders2[[1]](10)
adders2[[10]](10)

add <- function(x) {
  x
  function(y) x + y
}

lapply(1:10, add)[[1]](10)

f <- function(x = ls()) {
  a <- 1
  x
}
f()
f(ls())

x <- NULL
if (!is.null(x) && x > 0) {
  
}

`&&` <- function(x, y) {
  if (!x) return(FALSE)
  if (!y) return(FALSE)
}
a <- NULL
!is.null(a) && a > 0

if (is.null(a)) stop("a is null")
!is.null(a) || stop("a is null")

### 6.4.5 ...

plot(1:5, col = "red")
plot(1:5, cex = 5, pch = 20)

plot(1:5, bty = "u")
plot(1:5, labels = FALSE)

f <- function(...) {
  names(list(...))
}
f(a = 1, b = 2)

sum(1, 2, NA, na.mr = TRUE)
# sum(1, 2, NA, na.rm = TRUE)

### 6.4.6

# x <- sample(replace = TRUE, 20, x = c(1:10, NA))
x <- sample(c(1:10, NA), 20, replace = TRUE)
y <- runif(min = 0, max = 1, 20)
y <- runif(n = 20, min = 0, max = 1)
cor(x = x, y = y, method = "k", use = "p")

# 2
f1 <- function(x = {y <- 1; 2}, y = 0) {
  x + y
}
f1()


f2 <- function(x = z) {
  z <- 100
  x
}
f2()

## 6.5 Special calls

### 6.5.1 Infix functions

`%+%` <- function(a, b) paste0(a, b)
"new" %+% " string"

"new" %+% " string"
`%+%`("new", " string") #as same as the above

1 + 5

`+`(1, 5)

`% %` <- function(a, b) paste(a, b)
`%'%` <- function(a, b) paste(a, b)
`%/\\%` <- function(a, b) paste(a, b)
"a" % % "b"
"a" %'% "b"
"a" %/\% "b"

`%-%` <- function(a, b) paste0("(", a, " %-% ", b, ")")
"a" %-% "b" %-% "c"

`%||%` <- function(a, b) if (!is.null(a)) a else b
# function_that_might_return_null() %||% default value

### 6.5.2 Replacement functions

`second<-` <- function(x, value) {
  x[2] <- value
  x
}
x <- 1:10
second(x) <- 5L
x

library(pryr)
x <- 1:10
address(x)
second(x) <- 6L
address(x)

x <- 1:10
address(x)
x[2] <- 7L
address(x)

`modify<-` <- function(x, position, value) {
  x[position] <- value
  x
}
modify(x, 1) <- 10
x

x <- `modify<-`(x, 1, 10)
# modify(get("x"), 1) <- 10

# get("x") <- `modify<-`(get("x"), 1, 10)

x <- c(a = 1, b = 2, c = 3)
names(x)
names(x)[2] <- "two"
names(x)

`*tmp*` <- names(x)
`*tmp*`[2] <- "two"
names(x) <- `*tmp*`

xor

### 6.5.3 Exercises

# 3
`% or %` <- function(x, y) {
  (x | y) & !(x & y)
}
x <- sample(1:10, 3, replace = T)
y <- sample(3:12, 3, replace = T)
x % or % y

# 4
intersect

`%it%` <- function(x, y) {
  y <- as.vector(y)
  unique(y[match(as.vector(x), y, 0L)])
}

ggplot2::mpg$hwy %it% ggplot2::mpg$cty

# 5

`modify<-` <- function(x, value) {
  x[sample(length(x), 1)] <- value
  x
}

x <- 1:10
modify(x) <- 7

## 6.6 Return values

f <- function(x) {
  if (x < 10) {
    0
  } else {
    10
  }
}
f(5)
f(15)

f <- function(x, y) {
  if (!x) return(y)
  
  # complicated processing here
}

f(F, 10)


f <- function(x) {
  x$a <- 2
  x
}
x <- list(a = 1)
f(x)
x$a

f1 <- function() 1
f2 <- function() invisible(1)
f1()
f2()
f1() == 1
f2() == 1

(f2())

a <- 2
(a <- 2)

a <- b <- c <- d <- 2
(a <- (b <- (c <- (d <- 2))))

### 6.6.1 On exit

in_dir <- function(dir, code) {
  old <- setwd(dir)
  on.exit(setwd(old))
  
  force(code)
}
getwd()
in_dir("~", getwd())

getwd()
?source()

capture.output

capture.output2 <- function(code) {
  temp <- tempfile()
  on.exit(file.remove(temp), add = TRUE)
  
  sink(temp)
  on.exit(sink(), add = TRUE)
  
  force(code)
  readLines(temp)
}

capture.output2(cat("a", "b", "c", sep = "\n"))
capture.output(cat("a", "b", "c", sep = "\n"))

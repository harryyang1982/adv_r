#2.1 Introduction

#quiz
df <- data.frame(runif(3), runif(3))
names(df) <- c(1, 2)

df$`3` <- sum(df$`1`, df$`2`)
df

x <- runif(1e6)
y <- list(x, x, x)

a <- c(1, 5, 3, 2)
b <- a
b[[1]] <- 10
b

## Prerequisites
library(lobstr)

## Sources

#2.2 Binding basics

x <- c(1, 2, 3)
y <- x

obj_addr(x)
obj_addr(y)

##2.2.1 Non-syntactic names

# _abc <- 1
# if <- 10

`_abc` <- 1
`_abc`

`if` <- 10
`if`
# if is different from `if`

# "if" <- 10
# "if"
# `if`

# don't dare use "" while assigning non-syntactic names

##2.2.2 Exercises

#1
a <- 1:10
b <- a
c <- a
d <- 1:10

obj_addr(a)
obj_addr(b)
obj_addr(c)
obj_addr(d) # d is different from a, b and c

#2 - verifying function object
mean
base::mean
get("mean")
evalq(mean)
match.fun("mean")

obj_addr("mean")
obj_addr(`mean`)

#3
?read.csv
## colnames
?make.names

make.names(".123e1")

#2.3 Copy-on-modify

x <- c(1, 2, 3)
y <- x

y[[3]] <- 4
x

##2.3.1 tracemem()
x <- c(1, 2, 3)
cat(tracemem(x), "\n")
# cat(tracemem(x))

y <- x
y[[3]] <- 4L
y[[3]] <- 5L

untracemem(y)

##2.3.2 Function calls
f <- function(a) {
  a
}

x <- c(1, 2, 3)
cat(tracemem(x), "\n")

z <- f(x)
# there's no copy here!

untracemem(x)

#2.3.3 Lists

l1 <- list(1, 2, 3)

l2 <- l1
l2

l2[[3]] <- 4

ref(l1, l2) # shallow copy

##2.3.4 Data frames
d1 <- data.frame(x = c(1, 5, 6), 
                 y = c(2, 4, 3))
d1
obj_addr(d1)

d2 <- d1
d2[, 2] <- d2[, 2] * 2

d3 <- d1
d3[1, ] <- d3[1, ] * 3
d3
obj_addr(d3)

##2.3.5 Character vectors

x <- c("a", "a", "abc", "d")
x
ref(x, character = TRUE)
# ref(x)

##2.3.6 Exercises

#1
cat(tracemem(1:10), "\n") #cause just prints out single values

#2
x <- c(1L, 2L, 3L)
tracemem(x)

x[[3]] <- 4 # cause is different not only value but also class

#3
a <- 1:10
b <- list(a, a)
c <- list(b, a, 1:10)
c

#4
x <- list(1:10)
x[[2]] <-  x
x

#2.4 Object size
obj_size(letters)
obj_size(ggplot2::diamonds)

x <- runif(1e6)
obj_size(x)

y <- list(x, x, x)
obj_size(y)

obj_size(list(NULL, NULL, NULL))

banana <- "bananas bananas bananas"
obj_size(banana)
obj_size(rep(banana, 100))

obj_size(x, y)
obj_size(1:3)

obj_size(1:1e3)
obj_size(1:1e6)
obj_size(1:1e9)

##2.4.1 Exercises

#1
y <- rep(list(runif(1e4)), 100)
object.size(y)
obj_size(y)

?object.size

#2
funs <- list(mean, sd, var)
obj_size(funs)
obj_size(mean)
obj_size(sd)
obj_size(var)

#3
a <- runif(1e6)
obj_size(a)

b <- list(a, a)
obj_size(b)
obj_size(a, b)

b[[1]][[1]] <- 10
obj_size(b)
obj_size(a, b)

b[[2]][[1]] <- 10
obj_size(b)
obj_size(a, b)

#2.5 Modify-in-place

##2.5.1 Objects with a single binding
v <- c(1, 2, 3)
v[[3]] <- 4

#object id is not changed

x <- data.frame(matrix(runif(5 * 1e4), ncol = 5))
medians <- vapply(x, median, numeric(1))

for (i in seq_along(medians)) {
  x[[i]] <- x[[i]] - medians[[i]]
}
cat(tracemem(x), "\n")

for (i in 1:5) {
  x[[i]] <- x[[i]] - medians[[i]]
}

untracemem(x)

y <- as.list(x)
cat(tracemem(y), "\n")

for (i in 1:5) {
  y[[i]] <- y[[i]] - medians[[i]]
}

##2.5.2 Environments
e1 <- rlang::env(a = 1, b = 2, c = 3)
e2 <- e1

e1$c <- 4
e2$c

e <- rlang::env()
e$self <-  e

##2.5.3 Exercises

#1
x <- list()
x[[1]] <- x
x

#2
#3

#2.6 Unbinding and the garbage collector
x <- 1:3

x <- 2:4

rm(x)
gc()

mem_used()


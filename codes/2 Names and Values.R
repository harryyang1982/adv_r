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

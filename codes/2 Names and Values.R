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

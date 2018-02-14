install.packages("dplyr")
install.packages("plyr")
install.packages("microbenchmark")

library(dplyr)
library(microbenchmark)
library(plyr)

##################### 1. R UNDER THE HOOD AS A PROGRAMMING LANGUAGE #####################
# R is considered a high-level, interpreted language, which does a lot of work for you.
# For instance,
x <- 1.0
# With this line, R takes care of 1) assigning 1.0 to a float number, 2) making x a numeric 
# datatype, 3) storing the variable as binary in memory as "x"

# Or let's take:
x <- "foo"
# R takes care of updating the variable to a string, which is stored as a sequence of 
# characters.
# Overall, this means that R takes relatively longer than lower-level languages, like C or C++,
# where the variable is defined by the user like:
# int x = 0;
# This is faster since C or C++ doesn't have to figure out what data type x is, but errors
# will be thrown up if you try to reassign it to 5.5 since you need to explicitly change 
# any details of the variable.

# The key difference between R and C/C++ is whether the language is "compiled" beforehand.
# R is not compiled, so does many computer tasks on the fly. However, C/C++ does it during
# the compilation stage before being allowed to run the code, which optimizes the binary 
# code conversion.
# If you look closer at many R functions, you may also find that they are written in lower
# languages and have a R "wrapper", like fft:
fft
# which tends to make these functions faster since R has to interpret the input of the function
# before running the compiled C code. This is why similar libraries might have 
# noticable differences in speed. 

# THIS is where vectorization comes in for R code.
# If you want to run the fft() function on numerous elements in a vector, you can either
# 1) pass the whole vector through the function once or 
fft(c(1,2,3))
# 2) call the function for every element of the vector
y <- c()
for (i in 1:3){
  y[i] <- i
  if (length(y)==3) {
    y <- fft(y)
  }
}
# The latter is less effective since we need to run the R code for every element instead of
# once through the compiled C code. Loops are inevitably used in any code (compiled or not)
# though, but much quicker to have the comp. processing occur in C than R.
# Also, it is important to note that R has some restrictions in an effort to increase 
# its speed, such as vectors being only 1 data type (numeric, string, etc.).

# This all results in a case where shorter R code is typically short, whereas C/C++ code
# tends to be very long and consist of many short, simple statements. Thus, the first 
# method introduces the notion of vectorization in R.


##################### 2. PRE-ALLOCATING MEMORY & SPEED TESTING IN R ####################
## 2a. TEST METHOD 1 - system.time()
num.vec <- numeric(0)     # numeric class
known.vec <- rep(NA, 1e6) # pre-allocated vector (if you know)
appends <- function(vec, n) {
  for (i in 1:n) {
    vec[i] <- i
  }
}
system.time(appends(num.vec, 1e6)) # numeric vector
system.time(appends(known.vec, 1e6)) # pre-allocated vector
# Notice how pre-allocated is faster due to not having to copy the vector for 
# every iteration. 

# NOTE: Only noticable for large # of iterations; it ultimately is a tradeoff
# between speed and memory (e.g. can your RAM hold very large vectors >10,000,000 elements)
small.num.vec <- c()
small.known.vec <- rep(NA,1e4)
system.time(appends(small.num.vec, 1e4))
system.time(appends(small.known.vec, 1e4))


## 2b. MORE COMPLEX TEST METHOD 2 - microbenchmark()
## The microbenchmark package runs a set of functions n times to get a statistical
## comparison of function performance

# same function as above to append numbers to a vector
vec.append <- function(vec) {
  for (idx in 1:1e4) {
    vec[idx] <- idx
  }
}
num.vec <- numeric(0)       # numeric class
num <- 1                    # class(1) reveals numeric vector
known.vec <- rep(NA, 1e4) # pre allocted vector of 10,000 NA

#################### 3. BENCHMARKING ####################
# Test 1: pre-allocation
compare1 <- microbenchmark::microbenchmark(vec.append(num.vec),vec.append(num),
                                           vec.append(known.vec),times=100)
microbenchmark::autoplot.microbenchmark(compare1)

# Test 2: 3 common methods to add elements to vectors 

# BAD PRACTICE: This appends 1 number to the vector AND copies it to a separate variable (1 input, 2 outputs)
f1.slow <- function (n) {
  l <- list()
  for(idx in 1:n) {
    l <- append(l, idx)
  }
  return(l)
}
# BETTER PRACTICE: This appends 1 number to the list for each iteration (1 input, 1 output), however,
# tkhe use of length() slows down this code 
f2.faster <- function (n) {
  l <- list()
  for(idx in 1:n) {
    l[[length(l) + 1]] <- idx
  }
  return(l)
}
# BEST PRACTICE: Creates a pre-allocated vector and simple uses index number to minimally append the numbers
f3.fastest <- function (n) {
  l <- vector("list", n)
  for(idx in 1:n) {
    l[[idx]] <- idx
  }
  return(l)
}
n <- 1e4
compare2 <- microbenchmark::microbenchmark(f1.slow(n),f2.faster(n),
                                           f3.fastest(n),times=25)
microbenchmark::autoplot.microbenchmark(compare2)
# Simple changes in how you implement codes can make a big difference in speed, but this can vary widely
# between tasks. 

#################### 4. Conditional formmating ####################
## QUESTION: How should we write a function that will convert vector of 
## negative angles to positive angles (add 360 degs)?

# dataframe of 3 columns with 10,000 random angles from -360 to 360 deg
data <- data.frame(replicate(3, sample(-360:360, 1e4, rep=TRUE)))

# A naive programmer might try to solve this problem by applying the 
# condition for each row via a for loop
test1 <- system.time(
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      if (data[i,j] < 0) {
        data[i,j] <- data[i,j] + 360
      } else data[i,j]
    }
  }
)
# However, we can apply a simple conditional masking statement to apply per column.
# base R
data <- data.frame(replicate(3, sample(-360:360, 1e4, rep=TRUE)))
test2 <- system.time(
  for (j in 1:3) {
    data[data[j] <= 0,j] <- data[data[j] <= 0,j] + 360
  }
)
# dplyr method
data <- data.frame(replicate(3, sample(-360:360, 1e4, rep=TRUE)))
test3 <- system.time(
  data %>% mutate(X1 = ifelse(X1 < 0, X1+360, X1),
                  X2 = ifelse(X2 < 0, X2+360, X2),
                  X3 = ifelse(X3 < 0, X3+360, X3))
)

# check results
rbind(test1,test2,test3)
# As we can see with this simple task, writing effective code makes a big difference
# in 1) speed and 2) readability


#################### 5. Tracing/Profiling code ####################
# Benchmarking is useful for comparing single functions, but profiliing with Rprof()
# is best for determining the slow parts of a script. Rprof() works by sampling the 
# list of current running functions and the fucntion that called it, known as the 
# "call stack". Rprof() samples this call stack at regulat intervals to determine
# where either a function is 1) repeatedly called or 2) being slow.

# random walk function to test 
rw2s1 <- function(n) {
  xpos = ypos = numeric(n)
  xdir = c(TRUE, FALSE)
  pm1 = c(1, -1)
  for (i in 2:n) if (sample(xdir, 1)) {
    xpos[i] = xpos[i - 1] + sample(pm1, 1)
    ypos[i] = ypos[i - 1]
  } else {
    xpos[i] = xpos[i - 1]
    ypos[i] = ypos[i - 1] + sample(pm1, 1)
  }
  list(x = xpos, y = ypos)
}

# sets up a profiler with some "filename"
Rprof("out.out")
# code to profile over some # of iterations
for (i in 1:1000)
  pos = rw2s1(1000)
# end sampling
Rprof(NULL)
# summary table 
summaryRprof("out.out")
# This table is kind of difficult to read for larger scripts, so smarter people on 
# StackOverflow have written a nicer function to more intuitively read the results
# Source: https://goo.gl/mHrweA
source("proftable.R")
# function proftable()
proftable("out.out")
# The sample.int function inside sample() takes a majority of the time. 
# You can see that this table summarizes the profile report quite nicely by showing
# absolute path of the functions being analyzed and their time contribution. It 
# also lists the "parent call", which is the top function of the call stack. The 
# blank 3rd row refers to this parent call function.

### MATRIX EXAMPLE - Profile/Profvis tool
for (i in 1:1000) {
  X <- rnorm(100000)
  X <- matrix(X, ncol = 10)
  
  Y <- rnorm(10000, colSums(X))
  
  XtX <- t(X) %*% X
  XtXInv <- solve(XtX)
  
  XtY <- t(X) %*% Y
  betaHat <- XtXInv %*% XtY
  betaHat2 <- coef(lm(Y ~ X - 1))
}


# Sources for further reading
# http://www.noamross.net/blog/2014/4/16/vectorization-in-r--why.html
# http://www.noamross.net/blog/2013/4/25/faster-talk.html
# http://www.burns-stat.com/pages/Tutor/R_inferno.pdf
# http://musicallyut.blogspot.com/2012/07/pre-allocate-your-vectors.html
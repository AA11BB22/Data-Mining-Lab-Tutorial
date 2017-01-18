# 1. Predict the result of the following R statements. 
#    Only after your prediction, run it in R

exp(-Inf)
# exponential graph will not reach 0, -Inf would give a value very close to 0
# which we is good enough to say it is equivalent to 0.
NA == NA # NA
NA == NULL # logical(0)
NULL == NULL # logical(0)
NA & FALSE # FALSE


# 3. 
# a. Load the warpbreaks dataset. 
#    Find out which columns of warpbreaks are either numeric or integer in a single command
df <- warpbreaks
sapply(df, is.numeric) # returns a logical vector
str(df) # info about the data

# c. Figure out what is the underlying type of an object for mean.
#    Try the following command mean[1] and then try and use typeof() to discover the answer.
typeof(mean(df$breaks))


# 4.
v <- factor(c("2", "3", "5", "7", "11"))
v.char <- as.character(v)
v.char <- as.numeric(as.character(v)) # as.numeric alone would return the levels' value


# 5.  We will try and work with an irregular text file
# a. Read example.txt from the files folder. What is the content of example.txt?
(plain <- readLines("files/example.txt"))

# b. Separate the vector of lines into a vector containing comments and a vector containing data. Hint : Use grep.
(example.comments <- grep("^//", plain, value = TRUE))
(example.data <- grep("^[^/]|^/[^/]", plain, value = TRUE)) 
# The latter regex, Find me something
# [starting with NOT /] OR [starting with / and NOT / next]
# Since comment requires at least //, / is technically not a comment.

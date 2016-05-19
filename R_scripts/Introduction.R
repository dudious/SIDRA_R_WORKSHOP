###############################################################################
## Data Science in Biomedical Research:
## From the Basics of R coding to Advanced Machine Learning
## May 18 and 19, 2016,  Sidra Research
##
## David Furman (dfurman@sidra.org) & Wouter Hendrickx (whendrickx@sidra.org)
## Research, Translational Medicine, Sidra Medical and Research Institute
###############################################################################

# nice workshop and lecture

###############################################################################
###############################################################################
##Basic R

# This is a comment

#Constants
3 + 2

# <- and = are equivalent as assignment operator
a <- 3
a
b = 2
b

#R as caluclator
a + b
c <- b + 2 * a + 0.3
c
print(c)

a <- b
a

#Help for a function in R:
?print
help(print)
#HTML based global help" (open the R manual)
help.start()


###############################################################################
##Numeric (floating point)
x <- 3.14159
x
class(x)

##Vectors and indexing
#To define a vector of numbers, use c()
x <- c(3, 5, 2, 4)
x
#Selecting parts/elements of a vector
x[2] #Access the second element
x[2] <- 10 #Replace the second element
x
#Variables can be used as indices
i <- c(2, 4)
x[i]

#Many operations operate element-wise on vectors and returning vectors
x + 2

#Number of elements in vector x
length(x)
#Sort the values in the vector x
x.sorted <- sort(x)

#Order of the elements in the vector x
order(x)
x.ordered <- x[order(x)]


#Easy way to build vectors of consecutive numbers
2:7
#Sequence from 1 to 2 with stepsize 0.1
seq(from = 1, to = 2, by = 0.1)
#Remember, always use ?seq to get help for a function/operation

#Vector of repeated elements
rep(1, times = 4)
rep(c(5, 9), times = c(3, 2))


#Logical Operators (selection)
5 > 2 #greater than  (same for less (<))
3 <= 1:6 #less than or equal to  (same for greater (>=))
1:4 > 2
5 == 5 #exactly equal to
5 != 6 #not equal


##Index (objects of data type "logical")
x
x > 3 #Index (for each elements in x: is it > 3)
x[ x > 3 ] #Use of index -> "extract" the values from x that are > 3
#same using a stored index variable
index <- x >= 3
index
x[index == FALSE]
#Indices are of class "logical" -> values are TRUE or FALSE (or NA)
class(index)

#Assign symbolic names to the elements of a vector
names(x) <- c('a', 'b', 'c', 'd')
x
#Index elements of the vector using the symbolic names
x['c']
x[c('a', 'c')]

#Missing values (NA): often in biological data
d <- c(2:4, NA, 6)
d
d == NA #intuitive but doesn't give what expected
is.na(d)
which(is.na(d) == TRUE)
#This...
d[is.na(d)]
#is a short version for
d[is.na(d) == TRUE]
#We are not interested in the NA but in the numbers that are not NA:
d[is.na(d) == FALSE]
#... equals ...
d[!is.na(d)]


###############################################################################
##Character (string) and vector of strings
st <- "hello world"

w <- c("CD3-/CD19+", "CD3+/CD4+", "CD3-/CD19+", "CD3+/CD8+/CD38+")	
w
class(w)
w[2] <- NA
w
w[!is.na(w)] #equal to w[is.na(w)==FALSE]

w3 <- c(w, "CD3pos")
w3
w3 <- c(w3, 10)
w3

##Task: Multiply the 10 in vector w3 with 2.

w3[6]<-as.numeric(w3[6]) * 2
w3
###############################################################################
#Lists
#A collection of objects
mylist <- list()
mylist[["group"]] <- c("Control", "Treatment")
mylist[["numbers"]] <- c(1:5, 51:55)
mylist[["single"]] <- 19.57


#Another way to create a new list
x <- list(IDs=c(1, 2, 3), names=c('Control', 'Treatment1', 'Treatment2'), study="My Study")
x
names(x)

##Referencing elements of a list
mylist[[2]][1] #2nd component of the list
mylist[["numbers"]] #component named 'numbers' in list
mylist$numbers #component named 'numbers' in list




###############################################################################
#Manipulating character variables/vectors
#Split a string
s <- "CD3+/CD4+"
s
s1 <- strsplit(s, split="/", fixed=TRUE)
s1 #strsplit returned a list
#If we want a vector, use unlist() to convert the list into a vector
unlist(s1)
#However, when strsplit a vector, each split result of one element of the vector will be a list entry
#--> unlist probably not usefull:
s2 <- c(s, w)
s2
s3 <- strsplit(s2, split="/")
s3
s3[[2]][1]

#Substring
substr(s, 6, 9)

s
##Find a substring using grep:
grep("CD3+", s2, fixed=TRUE)
s2[grep("CD3+", s2, fixed=TRUE)]

##Unique
unique(s2) #on a vector
unique(s3) #on a list

##Pattern matching and replacement
s <- c("CD3+_CD4+")
s <- gsub("_", "/", s, fixed=TRUE)
s
##Number of characters
nchar(s)

###############################################################################
##paste: Concatenate Strings (combine elements into strings)
#"Population:" to the cell population definition (e.g. to plot as axis title, or in report etc.)
paste("Population:" ,s)
#Add "Live/"
paste("Live/", s)#There is a space between Live/ and CD3 that we don't want: define sep
paste("Live/", s, sep="")
#Alternatively, use "/" as separator
paste("Live", s, sep="/")
#Several strings can be combined
s.single <- "Singlets"
paste("Live", s.single ,s, sep="/")

#paste can be applied elementwise to a vector in one line
w2
paste("Live", w2, sep="/")

#Combine the elements of a vector by "_"
paste(w2, sep="_")#Does not give the desired result
#Use collapse
paste(w2, collapse="_")



###############################################################################
#Matrices
#Build a matrix with 2 columns and 3 rows
A <- matrix(c(1, 4, 7, 2, 4, 7, 5, 10, 2, 11, 6, 8), ncol=3)
A
matrix(1:12, ncol=3)
nrow(A) #Number of rows
ncol(A) #Number of columns
dim(A) #Dimension of matrix
#Indexing:
A[1, 2] #First number: row number, second number: column number
A[1, 2] <- 1.1
A
A[ ,2] #Retrieve column 2
A[ ,2] < 7
A[A[ ,2] < 7, ] #Get all rows which value in column 2 are less than 7
#Get all values that are equal or larger than 2 from column 1 of A
A[ A[ ,1] >= 2, 1]

##Task: Get the values of column 1 and 2, for all rows that have a value greater
##or equal to 8 in column 3


##Task: Generate a submatrix of the columns 2 and 3,
##in the decreasing order of values in column 1.



##
class(A)
class(A[,2])

#Transpose A
t(A)

#Combine matrices
B <- matrix(1.5, ncol=3, nrow=4)
B
cbind(A, B) #column wise
rbind(A, B) #row wise

#Row and column names
colnames(A) <- c("SampleA", "SampleB", "SampleC")
A
rownames.tmp <- paste("Gene_", 1:4, sep="") ##?paste
rownames.tmp
rownames(A) <- rownames.tmp
A
colnames(A)
A[ ,'SampleB']


#Matrices can also store characters
C <- matrix(c("A", "B", "C", "D"),ncol=2)
C
#All columns in a matrix must have the same mode (numeric, character, etc.) and the same length.

#When adding a character to a matrix with elements of mode numeric, all entries in the matrix will be converted to mode character
A[1, 1] <- "h"
A
class(A) <- "numeric" ##Notice the warning and check the values in A

##Arrays are similar to matrices but can have more than two dimensions.


###############################################################################
##Data frames
#Generalization of matrix:
#Columns can have different modes (numeric, character etc.)
d <- c(1:8)
e <- c("red", "white", "red", "red", "red", "white", NA, NA)
f <- c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)
g <- factor(c("female", "female", "male", "female", "male", "female", "female", "male"))
mydata <- data.frame(ID = d, Sex = g ,Color = e, Passed = f)
mydata


summary(mydata)


###############################################################################
##Factors - categorical values
#variable 'gender' with 20 "male" entries and 
#30 "female" entries 
gender <- c(rep("male", 20), rep("female", 30)) 
gender <- factor(gender) 
# stores gender as 20 1s and 30 2s and associates
# 1=female, 2=male internally (alphabetically)
# R now treats gender as a nominal variable 
levels(gender)
summary(gender)


##table function Counts the occurences
table(gender)
table(mydata[ ,"Sex"])
table(mydata[ ,"Passed"])##NAs are not included
table(mydata[ ,c("Sex","Passed")])


##factor data type: used to represent categorical data/discrete variables
#e.g. the variable "sex" with the two levels male or female.
#Factors are used for discrete variables in data.frames if they get analyzed in functions



###############################################################################
###############################################################################
###############################################################################
#A few basic mathematical functions
x <- c(3, 5, -2, 4)
mean(x)
median(x)
sum(x)
min(x)
max(x)
summary(x)
sd(x) #Standard Deviation
var(x) #Variance
sqrt(x) #square root
abs(x) #absolute value
sqrt(abs(x))
x^2

summary(x)

d <- c(2,3,4,NA,6)
mean(d)
mean(d, na.rm=TRUE) #Remove NA and build mean



###############################################################################
###############################################################################
###############################################################################
##if
5 > 2
if (5 > 2) print("5 is greater than 2")

i <- 5
if (i == 5) print("i is equal to 5")

# use of brackets
i <- 6
if (i != 5) { 
  print("i is not equal to 5")
}
# if / else
if (i > 10) {
	print("i is larger than 10")
} else {
	print("i is not larger than 10")
}

#Several conditions
#!x:      Not x
#x | y:   x OR y
#x & y:   x AND y
if (5 < 10 & 6 < 10) print("Yes, 5 less than 10 and 6 less than 10")
if (5 < 10 & 6 > 10) print("Yes")
if (5 < 10 | 6 > 10) print("Yes, at least one condition is TRUE") #| means OR
TRUE & TRUE
TRUE & FALSE
TRUE | FALSE


##Task (optional): learn about switch, ifelse etc. E.g. start on this page to get an overview:
##http://www.statmethods.net/management/controlstructures.html



###############################################################################
###############################################################################
###############################################################################
#for-loop
M <- matrix(1:30, ncol=3, byrow=TRUE)
colnames(M) <- paste("Sample", 1:ncol(M), sep=".")
rownames(M) <- paste("Gene", 1:nrow(M), sep='_')
#Build mean of each row
mean(M[1, ])
mean(M[2, ])
##...

##Do this in a for-loop:
#Create vector to store row means in
means <- rep(NA, nrow(M))
names(means) <- rownames(M)
means
for (i in 1:nrow(M)) {
  print(i)
  #Calculate mean and write into vector
  means[i] <- mean(M[i,], na.rm=TRUE)
  }
means

#rowMeans, colMeans actually do exist as function in R
rowMeans(M, na.rm=TRUE)


##Task: Build a loop:
#-Calculate row means of matrix M
#-Only save value in vector, when the mean is an odd number
#-If it is an even number, print number, comment that it is an even number and to which gene the current mean belongs




###############################################################################
###############################################################################
###############################################################################
##Functions intro

#Example 1:
calc.mean.function <- function(values) {
	#Calculate the mean
	res <- mean(values, na.rm=TRUE)
	#Return results
	return(res)
}
#Call this function
calc.mean.function(values = c(1 ,2, 3, 4, 5))
sampled.numbers <- sample(1:100, 10)
calc.mean.function(sampled.numbers)



#Example 2:
my.function <- function(p1 = 2, p2, p3) {#p1 has the default value 2
	res <- p1^2 + p2 * p3
	return(res)
}
#Call this function
my.function(p2 = 4, p3 = 1)#If p1 is not give in the function call, p1 will be "2" as given in the defintion of the function
my.function(p1 = 3, p2 = 4, p3 = 1)



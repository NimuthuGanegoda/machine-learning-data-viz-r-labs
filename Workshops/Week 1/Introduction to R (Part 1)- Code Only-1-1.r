## ------------------------------------------------------------------------
x <- 35


## ------------------------------------------------------------------------
x


## ------------------------------------------------------------------------
x <- 18
x


## ------------------------------------------------------------------------
y <- x + 8
y


## ------------------------------------------------------------------------
y <- 3 * x + 5
y


## ------------------------------------------------------------------------
diam <- 10 # diameter = 10 cm
radius <- diam / 2 # radius is half of the diameter
area.circle <- pi * radius^2 # area of the circle


## ------------------------------------------------------------------------
"a"
"abc123"
"apples"
"I hate apples"


## ------------------------------------------------------------------------
"a"
"abc"
"apples"
"I hate apples"


## ------------------------------------------------------------------------
5.5
2.75
pi


## ------------------------------------------------------------------------
5.5 + 2.7 # Addition
5.5 - 2.7 # Subtraction
5.5 / 2 # Division
5.5 * 2 # Multiplication
5.5^2 # Squaring
5.5^4 # To the power of 4
sqrt(5.5) # Square root
exp(5.5) # Exponential
log(5.5) # Natural log


## ------------------------------------------------------------------------
5L # 5
-3L #-3


## ------------------------------------------------------------------------
5 == 5 # Does 5 equal 5?
5 == 2 # Does 5 equal 2?
5 != 2 # Does 5 not equal 2?;
5 > 2 # Is 5 greater than 2?
5 >= 2 # Is 5 greater than or equals to 2
5 < 2 # Is 5 less than 2?
5 <= 2 # Is 5 less than or equals to 2


## ------------------------------------------------------------------------
(5 > 2) & (5 > 4)
(5 > 2) & (5 < 4)
(5 > 2) | (5 < 4)


## ------------------------------------------------------------------------
c() # A null vector
c(1, 2, 3) # A numeric vector
c("a", "b", "c") # A character vector
c(TRUE, FALSE, FALSE) # A logical vector


## ------------------------------------------------------------------------
a <- c(1:5)
a


## ------------------------------------------------------------------------
a[2] # 2nd element
a[3] # 3rd element
a[3:5] # 3rd to 5th element
a[c(2, 5)] # 2nd and 5th elements


## ------------------------------------------------------------------------
b <- c(6:10)
b # Create a new vector b containing the integers 6, 7, 8, 9 and 10.
c <- c(a, b)
c # Combine vectors a and b, and assign the vector sum to a new vector, c.


## ------------------------------------------------------------------------
a * 0.25 # Multiply the elements of vector a by a constant, i.e. 0.25
a + b # Add the elements of vector a and vector b together
b - a # Subtract the elements of vector a from vector b
a * b # Multiply the elements of vector a and vector b together
b / a # Divide the elements of vector b by the elements of vector a


## ------------------------------------------------------------------------
f1 <- c(1:5)
# A numeric vector
f1 <- factor(f1)
f1 # Convert f1 to a factor

f2 <- c("Male", "Female", "Female", "Male", "Female")
# A charactor vector
f2 <- factor(f2)
f2 # Convert f2 to a factor


## ------------------------------------------------------------------------
f3 <- factor(c("L", "M", "H", "H", "M", "L"))
f3


## ------------------------------------------------------------------------
# re-order the levels of f3 and overwrite the original f3.
f3 <- factor(f3, levels = c("L", "M", "H"))
f3


## ------------------------------------------------------------------------
Mat.A <- matrix(c(1:9), nrow = 3, ncol = 3, byrow = FALSE)
Mat.A


## ------------------------------------------------------------------------
Mat.B <- matrix(c(1:9), nrow = 3, ncol = 3, byrow = TRUE)
Mat.B


## ------------------------------------------------------------------------
Mat.A <- matrix(c(1:9), # 9 entries to be read in
  nrow = 3, # 3 rows
  ncol = 3, # 3 columns
  byrow = FALSE # data to be read in column-wise (default)
)
Mat.A


## ------------------------------------------------------------------------
v1 <- c(1:3) # Vector 1
v2 <- c(4:6) # Vector 2
v3 <- c(7:9) # Vector 3

Mat.A <- cbind(v1, v2, v3)
Mat.A # binding vectors as column vectors
Mat.B <- rbind(v1, v2, v3)
Mat.B # binding vectors as row vectors


## ------------------------------------------------------------------------
Mat.A * Mat.B


## ------------------------------------------------------------------------
Mat.A %*% Mat.B # A matrix multiply B
Mat.B %*% Mat.A # B matrix multiply A


## ------------------------------------------------------------------------
Mat.A[2, 3] # Access the element located in row 3 and column 3
Mat.A[1:2, 3] # Access the elements located in rows 1 and 2 along column 3
Mat.A[1, 2:3] # Access the elements located along row 1 and in columns 2 and 3
Mat.A[1, ] # Access all elements in row 1
Mat.A[, 3] # Access all elements in column 3
Mat.A[, c(1, 3)] # Access all elements in columns 1 and 3
Mat.A[, -1] # Access all data, except those in column 1


## ------------------------------------------------------------------------
Name <- c("John", "Sarah", "Zach", "Beth", "Lachlan")
# Name - Character vector
Age <- c(35, 28, 33, 55, 43)
# Age - Numeric vector
Gender <- factor(c("Male", "Female", "Male", "Female", "Male"))
# Gender - factor


## ------------------------------------------------------------------------
df <- data.frame(Name, Age, Gender)
df


## ------------------------------------------------------------------------
Coffee.Drinker <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
# Drinks coffee? - logical vector

data.frame(df, Coffee.Drinker) # Add new column to the data frame - Method 1
cbind(df, Coffee.Drinker) ## Add new column to the data frame - Method 2


## ------------------------------------------------------------------------
df[1, c(1:3)]
df[2:3, ]
df[, c(1, 3)]


## ------------------------------------------------------------------------
df[, "Name"]
df[, c("Name", "Gender")]


## ------------------------------------------------------------------------
df$Name # Access and display the "Name" column
df$Age # Access and display the "Age" column


## ------------------------------------------------------------------------
# Add the new variable, Coffee.Drinker, to df and recall the data frame
df$Coffee.Drinker <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
df

# Add the new variable, Diabetes, to df and recall the data frame
df$Diabetes <- factor(c("Yes", "No", "No", "No", "Yes"))
df


## ------------------------------------------------------------------------
tib1 <- data.frame(Name, Age, Gender, Coffee.Drinker)
tib1


## ------------------------------------------------------------------------
str(df)


## ------------------------------------------------------------------------
as.data.frame(tib1) # Convert the tibble, tib1, to a data frame
# as_tibble(df)  #Convert the data frame, df, to a tibble


## ------------------------------------------------------------------------
list1 <- list(c, Mat.A, df)
list1
str(list1) # examine the structure of the list and its components.


## ------------------------------------------------------------------------
list1[[1]] # Access the 1st component, i.e. vector c
list1[[2]] # Access the 2nd component, i.e. matrix Mat.A
list1[[3]] # Access the 3rd component, i.e. data frame df


## ------------------------------------------------------------------------
# Create the same list, but each component is labelled
list1 <- list(VecC = c, MatA = Mat.A, DatFrame = df)

str(list1) # Examine the structure of the list


## ------------------------------------------------------------------------
list1$VecC # Access the vector component
list1$MatA # Access the matrix component
list1$DatFrame # Access the data frame component


## ------------------------------------------------------------------------
c(1, "a") # numeric value is coerced into a character
c(TRUE, "a") # logical value coerced into a character
c(TRUE, 1) # logical value is coerced into a numeric; 1 = TRUE, 0 = FALSE by default

# The 1st three elements are coerced into characters.
matrix(c(5, FALSE, 4.6, "No"), nrow = 2, ncol = 2, byrow = FALSE)

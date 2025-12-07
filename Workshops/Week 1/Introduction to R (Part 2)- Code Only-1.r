# De-comment the line below to install the relevant packages (if required)
# install.packages("tidyverse","timeDate")
library(tidyverse)

## ------------------------------------------------------------------------
# 20 random numbers between 1 and 10 (inclusive)
x1 <- sample(1:10, size = 20, replace = TRUE)
x1
# 20 random numbers between 5 and 15 from a Uniform distributionn
x2 <- runif(20, 5, 15)
x2
# 20 random numbers from a normal distribution with mean=12 and sd=3
x3 <- rnorm(20, mean = 12, sd = 3)
x3


## ------------------------------------------------------------------------
# A sequence from 1 to 20 with a 0.5 increment
x4 <- seq(1, 20, by = 0.5)
x4
# Repeat the sequence {1,2,3,4,5} 5 times over
x5 <- rep(c(1:5), times = 5)
x5
# Repeat each element of the sequence {1,2,3,4,5} by 5 times
x6 <- rep(c(1:5), each = 5)
x6
# Repeat each element of the sequence {1,2,3,4,5} by 3 times, and then repeat
# that sequence the 2nd time. Note that "each" has precedence over "times".
x7 <- rep(c(1:5), times = 2, each = 3)
x7


## ------------------------------------------------------------------------
letters[1:10] # List first 10 letters (in lower case) of the alphabet
LETTERS[1:10] # List first 10 letters (in UPPER case) of the alphabet


## ----echo=FALSE,error=FALSE, out.width = "80%", fig.align='center', fig.cap='\\label{fig:fig4}Importing Data'----
# knitr::include_graphics("Importing_Data.png")  # Comment out - meant for R Markdown


## ---- echo=FALSE---------------------------------------------------------
# Load the ElderlyPopWA data
ElderlyPopWA <- read.csv("ElderlyPopWA-1-1-1.csv")
View(ElderlyPopWA)


## ---- echo=FALSE---------------------------------------------------------
# This section is no longer needed as data is loaded above
# Keeping for reference only


## ---- eval=FALSE---------------------------------------------------------
## #Method 1
## write.csv(ElderlyPopWA,  #Name of the data frame/tibble to be exported
##          "ElderlyPopWA_updated.csv"  #Name of the new CSV file to write the data to.
##          )
##
## #Method 2
## write_csv(ElderlyPopWA,  #Name of the data frame/tibble to be exported
##          "ElderlyPopWA_updated.csv"  #Name of the new CSV file to write the data to.
##          )


## ----echo=FALSE,error=FALSE, out.width = "80%", fig.align='center', fig.cap='\\label{fig:fig5}BMI classification'----
# knitr::include_graphics("BMI_Class.png")  # Comment out - meant for R Markdown

## ------------------------------------------------------------------------
# Create BMI categories for the elderly female participants

mBMI <- max(ElderlyPopWA$BMI) # maximum BMI value within the sample

ElderlyPopWA$BMI.class <- cut(ElderlyPopWA$BMI,
  breaks = c(0, 23, 31, mBMI), # Set the intervals for the classes
  labels = c(
    "Underweight",
    "Healthy_Weight",
    "Overweight"
  )
) # Add labels to the classes


  ## ------------------------------------------------------------------------
  # Subsetting by a categorical variable

  # Underweight individuals only
  Underweight <- subset(ElderlyPopWA, BMI.class == "Underweight")
  # Under and overweight individuals
  Unhealthy.weight <- subset(ElderlyPopWA, BMI.class == "Underweight" | BMI.class == "Overweight")
  # Subsetting by a condition on a continuous variable
  Age.LT75 <- subset(ElderlyPopWA, Age < 75)
  # Select those under 75 years of age.


  ## ------------------------------------------------------------------------
  # Underweight individuals only
  Underweight <- ElderlyPopWA[which(ElderlyPopWA$BMI.class == "Underweight"), ]

  # Select those under 75 years of age.
  Age.LT75 <- ElderlyPopWA[which(ElderlyPopWA$Age < 75), ]

  # Select those that are not under 75 years of age.
  Age.GTE75 <- ElderlyPopWA[-which(ElderlyPopWA$Age < 75), ]


  ## ------------------------------------------------------------------------
  # Measures of centre
  mean(ElderlyPopWA$Age) # Mean (i.e. average) age
  median(ElderlyPopWA$Age) # Median age

  # Measures of spread
  sd(ElderlyPopWA$Age) # Standard deviation of age
  range(ElderlyPopWA$Age) # range of age, i.e. minimum and maximum
  IQR(ElderlyPopWA$Age) # Interquartile range
  fivenum(ElderlyPopWA$Age) # Five-number summary, i.e. min, Q1, Q2, Q3, max.


  ## ------------------------------------------------------------------------
  # install.packages("moments");  #De-comment to install the package
  library(moments) # Load the pacakge

  # Measures of shape
  skewness(ElderlyPopWA$Age)
  kurtosis(ElderlyPopWA$Age)


  ## ------------------------------------------------------------------------
  colMeans(data.frame(ElderlyPopWA[, 2:8])) # Column means


  ## ------------------------------------------------------------------------
  apply(ElderlyPopWA[, 2:8], # Data
    MARGIN = 2, # Apply the function in a column-wise manner. MARGIN = 1 implies row-wise.
    FUN = mean # The function to be applied.
  )


  ## ------------------------------------------------------------------------
  apply(ElderlyPopWA[, 2:8], # Data
    MARGIN = 2, # Apply the function in a column-wise manner. MARGIN = 1 implies row-wise.
    FUN = sd # The function to be applied.
  )


  ## ------------------------------------------------------------------------
  BMI.freq <- table(ElderlyPopWA$BMI.class)
  BMI.freq # Number of participants in each BMI class
  BMI.prop <- prop.table(BMI.freq)
  BMI.prop # Proportions of sample for each BMI class


  ## ------------------------------------------------------------------------
  # Create another categorical variable, i.e. age group.
  ElderlyPopWA$Age.grp <- cut(ElderlyPopWA$Age, c(0, 74.99, 100),
    labels = c("<75years", "75+years")
  )

  # Cross tabulate age group with BMI
  tab <- table(ElderlyPopWA$Age.grp, ElderlyPopWA$BMI.class)
  tab # 2 by 2 contigency table
  prop.table(tab, 1) # Proportions by row, i.e. within each age group
  prop.table(tab, 2) # Proportions by column, i.e. within each BMI class
  tab / sum(tab) # Proportions relative to overall sample size


  ## ------------------------------------------------------------------------
  # Summarise the waist circumference of the individuals across the three BMI classes

  # Compute the mean
  aggregate(Waist ~ BMI.class, # Formula to subset the Waist data by BMI classes
    data = ElderlyPopWA, # Define the relevant data
    FUN = mean
  ) # Define the function to compute the desired statistic(s), i.e. mean

  # Compute the standard deviation
  aggregate(Waist ~ BMI.class, data = ElderlyPopWA, FUN = sd)


  ## ------------------------------------------------------------------------
  # Summarise the waist circumference of individuals across all combinations between
  # BMI classes and age groups.

  # Compute the mean
  aggregate(Waist ~ BMI.class + Age.grp, data = ElderlyPopWA, FUN = mean)

  # Compute the standard deviation
  aggregate(Waist ~ BMI.class + Age.grp, data = ElderlyPopWA, FUN = sd)


  ## ---- warning=FALSE------------------------------------------------------
  # Mean waist circumference by BMI class
  tapply(ElderlyPopWA$Waist,
    INDEX = ElderlyPopWA$BMI.class,
    FUN = mean
  )

  # Mean waist circumference by BMI class and age group
  tapply(ElderlyPopWA$Waist,
    INDEX = list(ElderlyPopWA$BMI.class, ElderlyPopWA$Age.grp),
    FUN = mean
  )


  ## ------------------------------------------------------------------------
  round(prop.table(table(ElderlyPopWA$BMI.class)), 3)


  ## ------------------------------------------------------------------------
  BMI.freq <- table(ElderlyPopWA$BMI.class)
  # Number of participants in each BMI class
  BMI.prop <- prop.table(BMI.freq)
  # Proportions of sample for each BMI class
  round(BMI.prop, 3) # Display the proportions to 3 d.p.


  ## ------------------------------------------------------------------------
  ElderlyPopWA$BMI.class %>%
    table(.) %>%
    prop.table(.) %>%
    round(., digits = 3)


  ## ------------------------------------------------------------------------
  ElderlyPopWA$BMI.class %>%
    table() %>%
    prop.table() %>%
    round(digits = 3)

  ## ------------------------------------------------------------------------
  # Create a copy of ElderlyPopWA and store it as a data frame
  dat <- data.frame(ElderlyPopWA)

  # For loop that starts at 2
  for (J in 2:8)
  {
    mean(dat[, J]) %>% # Find the mean of the jth column
      print() # Print the mean
  }


## ------------------------------------------------------------------------
5 %>% round(pi, digits = .)


## ------------------------------------------------------------------------
Greeting <- function() {
  print("Hello! My name is XXXX")
}


## ------------------------------------------------------------------------
Greeting()


## ------------------------------------------------------------------------
add3 <- function(x) {
  x + 3
}


## ------------------------------------------------------------------------
add3(5)
add3(10)
add3(15)


## ------------------------------------------------------------------------
add_mult <- function(x, y) {
  sum_xy <- x + y # Calculate the sum of x and y
  prod_xy <- x * y # Calculate the product of x and y
  ratio_xy <- x / y # Calculate the ratio of x to y
}


## ------------------------------------------------------------------------
x <- 3
y <- 4
add_mult(x, y) %>% # Call the function
  print() # Print the value of the function


## ------------------------------------------------------------------------
add_mult <- function(x, y) {
  sum_xy <- x + y # Calculate the sum of x and y
  prod_xy <- x * y # Calculate the product of x and y
  ratio_xy <- x / y # Calculate the ratio of x to y

  c(sum_xy, prod_xy, ratio_xy) # A vector consisting of the sum, product and ratio
}


## ------------------------------------------------------------------------
add_mult(x = 3, y = 4)


## ------------------------------------------------------------------------
add_mult(y = 4, x = 3)


## ------------------------------------------------------------------------
add_mult(3, 4)


## ------------------------------------------------------------------------
x <- 5 # Set an arbitrary x value

# If statement
if (x %% 2 != 0) {
  cat(paste(x, " is an odd integer\n", sep = "")) # print command
}


## ------------------------------------------------------------------------
x <- 10 # Set an arbitrary x value

if (x %% 2 != 0) {
  cat(paste(x, " is an odd integer\n", sep = ""))
} else {
  cat(paste(x, " is an even integer\n", sep = ""))
}


## ------------------------------------------------------------------------
score <- 65 # Set an arbitrary score

if (score < 0) {
  print("Invalid score!") # Cannot have a negative score
} else if (score < 50) {
  print("Your final grade is N.")
} else if (score < 60) {
  print("Your final grade is P.")
} else if (score < 70) {
  print("Your final grade is C.")
} else if (score < 80) {
  print("Your final grade is D.")
} else if (score <= 100) {
  print("Your final grade is HD.")
} else {
  print("Invalid score!") # Cannot have a score greater than 100%
}


## ------------------------------------------------------------------------
# For loop
for (I in 1:10)
{
  print(I)
}


## ------------------------------------------------------------------------
# For loop
for (I in 1:100)
{
  # If statement to check if the number is divisible by 7
  if (I %% 7 == 0) {
    print(I)
  }
}


## ------------------------------------------------------------------------
count <- 0 # Set the count of numbers divisible by 13 to 0

for (I in 500:800)
{
  # 1st if statement: Checks if the number is divisible by 13
  if (I %% 13 == 0) {
    print(I) # Print the number
    count <- count + 1 # Add 1 to count
  }

  # 2nd if statement: Checks if the count of numbers divisible by 13 is 10.
  #                  If so, break from the for loop
  if (count == 10) {
    break
  }
}

# install.packages(c("tidyverse","timeDate","ggpubr"))  #De-comment the code to install
library(tidyverse)
library(ggpubr)

# Resolve script directory so relative paths work when run from VS Code.
script_dir <- tryCatch(dirname(normalizePath(sys.frames()[[1]]$ofile)),
  error = function(e) getwd()
)

## ---------------------------------------------------------------------------------------
Edu <- c(
  "Below high school", "High school graduate",
  "TAFE degree", "Bachelor's degree", "Postgraduate degree"
) # Highest education level
Freq <- c(46000, 120000, 75000, 50000, 9000) # Corresponding frequency
Perc <- Freq / sum(Freq) * 100
# Corresponding percentage

# Create the data frame
df.edu <- data.frame(Edu, Freq, Perc)

# Reorganise the factor levels for the variable Edu.
df.edu$Edu <- factor(Edu, levels = c(
  "Below high school", "High school graduate",
  "TAFE degree", "Bachelor's degree", "Postgraduate degree"
))
View(df.edu) # View the data frame


## ---------------------------------------------------------------------------------------
ggplot(df.edu, aes(x = Edu, y = Freq)) +
  geom_bar(stat = "identity")


## ---------------------------------------------------------------------------------------
ggplot(df.edu, aes(x = Edu, y = Freq)) +
  geom_bar(
    stat = "identity",
    colour = "red", # Colour of the border of the bars
    fill = "salmon", # Colour inside the bars
    alpha = 0.5
  ) # Transparency of the fill colour from 0 to 1


## ---------------------------------------------------------------------------------------
ggplot(df.edu, aes(x = Edu, y = Freq)) +
  geom_bar(
    stat = "identity",
    colour = "red", # Colour of the border of the bars
    fill = "salmon", # Colour inside the bars
    alpha = 0.5
  ) + # Transparency of the fill colour from 0 to 1
  coord_flip()


## ---------------------------------------------------------------------------------------
g1 <- ggplot(df.edu, aes(x = Edu, y = Freq)) +
  geom_bar(
    stat = "identity",
    colour = "red", # Colour of the border of the bars
    fill = "salmon", # Colour inside the bars
    alpha = 0.5
  ) # Transparency of the fill colour from 0 to 1


## ---------------------------------------------------------------------------------------
g1.clean <- g1 +
  theme_minimal(base_size = 14) + # Apply a built-in theme.
  ggtitle("Highest Level of Education Attained") + # Add a title
  theme(
    plot.title = element_text(hjust = 0.5), # Centre the title
    panel.grid = element_blank()
  ) + # Remove the grid lines
  ylab("Frequency") + # Change the y-axis label, i.e. vertical axis
  xlab("Highest Education") + # Change the x-axis label, i.e. horizontal axis
  # Add a carriage return using '\n' to the labels so that they fit the space
  scale_x_discrete(labels = c(
    "Below high\nschool", "High school\ngraduate",
    "TAFE\ndegree", "Bachelor's\ndegree",
    "Postgraduate\ndegree"
  ))
g1.clean


## ---------------------------------------------------------------------------------------
g2 <- ggplot(df.edu, aes(x = Edu, y = Freq, fill = Edu)) +
  geom_bar(stat = "identity", alpha = 0.7)
g2.clean <- g2 +
  theme_pubclean(base_size = 14) + # Apply a theme
  ylab("Frequency") + # Change the y-axis label
  labs(
    x = "", # Remove x-axis label
    fill = "Education"
  ) + # Change the legend title
  theme(
    axis.text.x = element_blank(), # Remove the tick labels
    axis.ticks.x = element_blank(), # Remove the tick marks
    legend.position = "right"
  ) # Shif the legend to the right-hand
g2.clean


## ---------------------------------------------------------------------------------------
set.seed(2)

# Total number of fish caught
fish_caught <- sample(0:15, # List of values to sample from
  size = 300, # Number of random values
  replace = TRUE, # Sampling with replacement
  prob = c(
    0.11, 0.1, 0.1, 0.09, 0.09, 0.09, 0.09, 0.08, 0.07,
    0.04, 0.04, 0.04, 0.03, 0.01, 0.01, 0.01
  )
) # probability weights

# Number of fishers at the time of sampling
num_fisher <- sample(1:6,
  size = 300, replace = TRUE,
  prob = c(0.06, 0.11, 0.16, 0.17, 0.22, 0.28)
)

# Time of day
ToD <- factor(rep(c("Morning", "Evening"), each = 150), levels = c("Morning", "Evening"))
# Create the data frame. Note that num_fisher is converted to a factor.
df.fish <- data.frame(num_fisher = as.factor(num_fisher), ToD, fish_caught)
View(df.fish)


## ---------------------------------------------------------------------------------------
Tot_fishers <- aggregate(fish_caught ~ num_fisher, data = df.fish, FUN = sum)
View(Tot_fishers)


## ---------------------------------------------------------------------------------------
g3 <- ggplot(Tot_fishers, aes(x = num_fisher, y = fish_caught)) +
  geom_bar(
    stat = "identity",
    colour = "red", # Colour of the border of the bars
    fill = "salmon", # Colour inside the bars
    alpha = 0.5
  ) # Transparency of the fill colour from 0 to 1

# Clean up the plot
g3.clean <- g3 +
  theme_pubclean(base_size = 14) + # This theme is from ggpubr
  ylab("Total Fish Caught") +
  xlab("Number of Fishers")
g3.clean


## ---------------------------------------------------------------------------------------
Tot_fishers.ToD <- aggregate(fish_caught ~ num_fisher + ToD, data = df.fish, FUN = sum)
View(Tot_fishers.ToD)


## ---------------------------------------------------------------------------------------
g4 <- ggplot(Tot_fishers.ToD, aes(x = num_fisher, y = fish_caught, fill = ToD, colour = ToD)) +
  geom_bar(stat = "identity", alpha = 0.5)

# Clean up the plot
g4.clean <- g4 +
  theme_pubclean(base_size = 14) +
  ylab("Total Fish Caught") +
  xlab("Number of Fishers") +
  labs(fill = "Time of Day", colour = "Time of Day") # Changing the legend title
g4.clean


## ---------------------------------------------------------------------------------------
g5 <- ggplot(Tot_fishers.ToD, aes(x = num_fisher, y = fish_caught, fill = ToD, colour = ToD)) +
  geom_bar(
    stat = "identity", alpha = 0.5,
    position = position_dodge2(width = 3, padding = 0.1)
  )

# Clean up the plot
g5.clean <- g5 +
  theme_pubclean(base_size = 14) +
  ylab("Total Fish Caught") +
  xlab("Number of Fishers") +
  labs(fill = "Time of Day", colour = "Time of Day")
g5.clean


## ---------------------------------------------------------------------------------------
# Function to calculate standard error of the mean
SE <- function(x) {
  SE <- sd(x) / sqrt(length(x))
}

mean_catch <- aggregate(fish_caught ~ num_fisher + ToD, data = df.fish, FUN = mean)
# Mean catch
SE_catch <- aggregate(fish_caught ~ num_fisher + ToD, data = df.fish, FUN = SE)
# SE of mean

# Calculate the lower bound (LB) and upper bound (UB) of the error bars and add to data frame
mean_catch$LB <- mean_catch[, 3] - SE_catch[, 3]
mean_catch$UB <- mean_catch[, 3] + SE_catch[, 3]

View(mean_catch) # View the data frame


## ---------------------------------------------------------------------------------------
g6 <- ggplot(mean_catch, aes(x = num_fisher, y = fish_caught, fill = ToD, colour = ToD)) +
  geom_bar(
    stat = "identity", alpha = 0.5,
    position = position_dodge2(width = 3, padding = 0.1)
  ) +
  geom_errorbar(aes(ymin = LB, ymax = UB, colour = ToD),
    width = 0.5, size = 1.25,
    position = position_dodge(0.9)
  )

# Clean up the plot
g6.clean <- g6 +
  theme_pubclean(base_size = 14) +
  ylab("Total Fish Caught") +
  xlab("Number of Fishers") +
  labs(fill = "Time of Day", colour = "Time of Day")
g6.clean


## ---------------------------------------------------------------------------------------
# Load ElderlyPopWA from the repo (avoids Desktop dependency)
elderly_path <- file.path(script_dir, "..", "Week 1", "ElderlyPopWA-1-1-1.csv") %>% normalizePath(mustWork = FALSE)
if (!file.exists(elderly_path)) {
  alt_path <- file.path(getwd(), "Workshops", "Week 1", "ElderlyPopWA-1-1-1.csv")
  if (file.exists(alt_path)) {
    elderly_path <- alt_path
  } else {
    stop("ElderlyPopWA-1-1-1.csv not found. Checked: ", elderly_path, " and ", alt_path)
  }
}
ElderlyPopWA <- read.csv(elderly_path, stringsAsFactors = TRUE)

ggplot(ElderlyPopWA, aes(x = BMI)) +
  geom_histogram() # Histogram with default settings


## ---------------------------------------------------------------------------------------
g7 <- ggplot(ElderlyPopWA, aes(x = BMI)) +
  geom_histogram(binwidth = 3, colour = "white", fill = "steelblue")

# Clean up the plot
g7.clean <- g7 +
  ylab("Frequency") +
  xlim(15, 40) + # Change the limits of the x-axis
  theme_pubclean(base_size = 14)
g7.clean


## ---------------------------------------------------------------------------------------
# Create BMI categories for the elderly female participants

mBMI <- max(ElderlyPopWA$BMI) # maximum BMI value within the sample

ElderlyPopWA$BMI.class <- cut(ElderlyPopWA$BMI,
  breaks = c(0, 23, 31, mBMI), # Set the intervals for the classes
  labels = c(
    "Underweight",
    "Healthy Weight",
    "Overweight"
  )
) # Add labels to the classes


## ---------------------------------------------------------------------------------------
g8 <- ggplot(ElderlyPopWA, aes(x = Pc_Body_Fat, fill = BMI.class)) +
  geom_histogram(
    colour = "white", # border colour of the bars
    alpha = 0.5, # Transparency of bars
    binwidth = 3, # width of the bins
    position = "identity"
  ) # default is stacked. Can be "identity" or "dodge"

# Clean up the plot
g8.clean <- g8 +
  theme_pubclean(base_size = 14) +
  labs(x = "Percent Body Fat", y = "Frequency", fill = "BMI Classification")
g8.clean


## ---------------------------------------------------------------------------------------
g9 <- ggplot(ElderlyPopWA, aes(x = Pc_Body_Fat, fill = BMI.class)) +
  geom_histogram(aes(y = ..density..), # Show densities instead of frequencies
    colour = "white", # border colour of the bars
    alpha = 0.5, # Transparency of bars
    binwidth = 3, # width of the bins
    position = "identity"
  ) # default is stacked. Can be "identity" or "dodge"

# Clean up the plot
g9.clean <- g9 +
  theme_pubclean(base_size = 14) +
  labs(x = "Percent Body Fat", y = "Density", fill = "BMI Classification")
g9.clean


## ---------------------------------------------------------------------------------------
g10 <- ggplot(ElderlyPopWA, aes(x = Pc_Body_Fat, fill = BMI.class)) +
  geom_density(colour = "white", size = 1, alpha = 0.5)

# Clean up the plot
g10.clean <- g10 +
  theme_pubclean(base_size = 14) +
  labs(x = "Percent Body Fat", y = "Density", fill = "BMI Classification")
g10.clean


## ---------------------------------------------------------------------------------------
g11 <- ggplot(ElderlyPopWA, aes(x = Pc_Body_Fat, fill = BMI.class)) +
  geom_histogram(aes(y = ..density..),
    colour = "white", # border colour of the bars
    alpha = 0.8, # Transparency of bars
    binwidth = 3, # width of the bins
    position = "identity"
  ) + # default is stacked. Can also be "dodge"
  geom_density(
    colour = "white", # Colour of the outline of the curves
    size = 0.5, # Width of the outline
    alpha = 0.3
  ) # Transparency of the curves

# Clean up the plot
g11.clean <- g11 +
  theme_pubclean(base_size = 14) +
  labs(x = "Percent Body Fat", y = "Density", fill = "BMI Classification")
g11.clean


## ---------------------------------------------------------------------------------------
data(iris) # Load the dataset, which is built
View(iris) # View the iris dataset


## ---------------------------------------------------------------------------------------
g12 <- ggplot(iris, aes(y = Sepal.Length)) +
  geom_boxplot(
    size = 1.5, # Thickness of the outline of the boxplot
    colour = "red",
    fill = "salmon",
    alpha = 0.5
  )

# Clean up the plot
g12.clean <- g12 +
  theme_pubclean(base_size = 14) +
  ylab("Sepal Length (cm)") +
  xlim(-1, 1) + # Widen the limits on x-axis, thus reducing the boxplot width
  theme(
    axis.ticks.x = element_blank(), # Remove the tick marks
    axis.text.x = element_blank()
  ) # Remove tick labels
g12.clean


## ---------------------------------------------------------------------------------------
g13 <- ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(aes(colour = Species, fill = Species),
    size = 1.25,
    alpha = 0.5,
    outlier.size = 5, # Size of the outlier(s), if any
    outlier.shape = 8
  ) # Shape of the outlier(s), if any

# Clean up the plot
g13.clean <- g13 +
  theme_pubclean(base_size = 14) +
  labs(x = "", y = "Sepal Length (cm)") +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )
# No need for label since there is a legend
g13.clean


## ---------------------------------------------------------------------------------------
g14 <- ggplot(iris, aes(x = Species, y = Sepal.Length, colour = Species, fill = Species)) +
  geom_boxplot(width = 0.7, size = 1.25, alpha = 0.5, outlier.size = 5, outlier.shape = 8) +
  geom_jitter(size = 1.5, position = position_jitter(0.1))

# Clean up the plot
g14.clean <- g14 +
  theme_pubclean(base_size = 14) +
  labs(x = "", y = "Sepal Length (cm)") +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )
g14.clean


## ---------------------------------------------------------------------------------------
g15 <- ggplot(iris, aes(x = Species, y = Sepal.Length, colour = Species, fill = Species)) +
  geom_boxplot(width = 0.2, size = 1.25, alpha = 0.5, outlier.size = 5, outlier.shape = 8) +
  geom_violin(alpha = 0.3, trim = FALSE)

# Clean up the plot
g15.clean <- g15 +
  theme_pubclean(base_size = 14) +
  labs(x = "", y = "Sepal Length (cm)") +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )
g15.clean


## ---------------------------------------------------------------------------------------
# Salary data: use repo file if present; otherwise synthesize a small sample so the code runs.
salary_path <- file.path(script_dir, "Salary_Data.csv") %>% normalizePath(mustWork = FALSE)
if (file.exists(salary_path)) {
  Salary_Data <- read.csv(salary_path)
} else {
  set.seed(42)
  Salary_Data <- data.frame(
    YearsExperience = seq(1, 10, length.out = 12),
    Salary = round(seq(45000, 120000, length.out = 12) + rnorm(12, 0, 4000))
  )
  message("Salary_Data.csv not found; using generated sample data.")
}
View(Salary_Data)


## ---------------------------------------------------------------------------------------
g16 <- ggplot(Salary_Data, aes(x = YearsExperience, y = Salary)) +
  geom_point(colour = "red", fill = "salmon", shape = 21, size = 4, alpha = 0.7)

# Clean up the plot
g16.clean <- g16 +
  theme_pubclean(base_size = 14) +
  xlab("Years of Experience") +
  ylab("Salary ($)")
g16.clean


## ---------------------------------------------------------------------------------------
data(airquality) # Load the air quality data
View(airquality) # View the dataset


## ---------------------------------------------------------------------------------------
aq <- na.omit(airquality) # Remove all missing cases


## ---------------------------------------------------------------------------------------
g17 <- ggplot(aq, aes(x = Solar.R, y = Ozone)) +
  geom_point(aes(size = Temp), colour = "cyan4", alpha = 0.5)

g17.clean <- g17 +
  theme_pubclean(base_size = 14) +
  xlab("Solar Radiation (lang)") +
  ylab("Ozone (ppb)") +
  labs(size = "Temp (Degrees F)")
g17.clean


## ---------------------------------------------------------------------------------------
set.seed(12)
Year <- c(2010:2019)
Australia <- sample(c(50:200), size = 10, replace = FALSE) %>%
  sort(., decreasing = FALSE)


df.breaches <- data.frame(Year, Australia)
View(df.breaches)


## ---------------------------------------------------------------------------------------
g18 <- ggplot(df.breaches, aes(x = as.factor(Year), y = Australia, group = 1)) +
  geom_line(colour = "red")

g18.clean <- g18 +
  theme_pubclean() +
  labs(x = "Year", y = "Number of Breaches")
g18.clean


## ---------------------------------------------------------------------------------------
g19 <- ggplot(df.breaches, aes(x = as.factor(Year), y = Australia, group = 1)) +
  geom_line(colour = "salmon") +
  geom_point(colour = "salmon", size = 3) +
  annotate("text", # Annotate plot with texts
    x = as.factor(Year), # x-position of text
    y = Australia + 10, # y-position of text
    label = Australia, # text labels
    colour = "blue4"
  ) # Colour of text labels

g19.clean <- g19 +
  theme_pubclean() +
  labs(x = "Year", y = "Number of Breaches") +
  ylim(0, 200)
g19.clean


## ---------------------------------------------------------------------------------------
set.seed(20)
df.breaches$Canada <- sample(c(20:150), size = 10, replace = FALSE) %>%
  sort(., decreasing = FALSE) # Fictitious data for Canada
View(df.breaches)


## ---------------------------------------------------------------------------------------
g20 <- ggplot(df.breaches, aes(x = as.factor(Year), group = 1)) +
  geom_line(aes(y = Australia), colour = "salmon") +
  geom_point(aes(y = Australia), colour = "salmon", size = 3) +
  geom_line(aes(y = Canada), colour = "cyan4") +
  geom_point(aes(y = Canada), colour = "cyan4", size = 3)

g20.clean <- g20 +
  theme_pubclean() +
  labs(x = "Year", y = "Number of Breaches") +
  ylim(0, 200)
g20.clean


## ---------------------------------------------------------------------------------------
g21 <- ggplot(df.breaches, aes(x = as.factor(Year), group = 1)) +
  geom_line(aes(y = Australia), colour = "salmon") +
  geom_point(aes(y = Australia), colour = "salmon", size = 3) +
  geom_area(aes(y = Australia), fill = "salmon", alpha = 0.5) +
  geom_line(aes(y = Canada), colour = "cyan4") +
  geom_point(aes(y = Canada), colour = "cyan4", size = 3) +
  geom_area(aes(y = Canada), fill = "cyan4", alpha = 0.5, density = 50)

g21.clean <- g21 +
  theme_pubclean() +
  labs(x = "Year", y = "Number of Breaches") +
  ylim(0, 200)
g21.clean


## ---------------------------------------------------------------------------------------
g4.clean +
  facet_wrap(vars(ToD)) +
  theme(legend.position = "") # Remove legend as it is now redundant


## ---------------------------------------------------------------------------------------
g4.clean +
  facet_wrap(vars(ToD), nrow = 2) +
  theme(legend.position = "") # Remove legend as it is now redundant


## ---------------------------------------------------------------------------------------
g4.clean +
  facet_grid(col = vars(ToD)) +
  theme(legend.position = "") # Remove legend as it is now redundant


## ---------------------------------------------------------------------------------------
g4.clean +
  facet_grid(row = vars(ToD)) +
  theme(legend.position = "") # Remove legend as it is now redundant


## ---------------------------------------------------------------------------------------
g4.clean +
  facet_grid(row = vars(ToD), scales = "free") +
  theme(legend.position = "") # Remove legend as it is now redundant


## ---------------------------------------------------------------------------------------
ggarrange(g16.clean, g15.clean, g11.clean,
  labels = c("A", "B", "C"),
  ncol = 2, nrow = 2
)
## ---------------------------------------------------------------------------------------
multiplot1 <- ggarrange(g16.clean,
  g15.clean + theme(legend.position = "right"),
  labels = c("A", "B"),
  ncol = 2, nrow = 1
)
multiplot2 <- ggarrange(multiplot1,
  g11.clean + theme(legend.position = "right"),
  labels = c("", "C"), nrow = 2
)
multiplot2


## ---------------------------------------------------------------------------------------
# Single plot
ggexport(multiplot2, filename = "Multiplots in one.pdf")

# Multiple plots across two pages, with 2 plots per page.
ggexport(g1.clean, g2.clean, g3.clean, g4.clean, # 4 plots
  nrow = 2, ncol = 1, # Arrange them vertically with 2 per page
  filename = "Multiplots.pdf"
)

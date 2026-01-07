# You may need to change/include the path of your working directory

# Import the dataset into R Studio
# (i) & (ii) Generate sub-sample and extract features
dat <- read.csv("WACY-COM.csv", na.strings=NA, stringsAsFactor=TRUE)

set.seed(10695889)

#Randomly select 400 rows
selected.rows <- sample(1:nrow(dat), size=400, replace=FALSE)

# Your sub-sample of 400 observations
mydata <- dat[selected.rows, ]

dim(mydata) # check the demension of your sub-sample

# Extract only Numeric Features
my_extracted_data <- mydata[, c("Hits",
                                "Average.Request.Size.Bytes",
                                "Average.Window.Seconds",)]
                                "Average.Attacker.Payload.Entropy.Bits",
                                "Average.ping.to.attacking.IP.milliseconds",
                                "Average.ping.variability",
                                "Individual.URLs.requested",
                                "APT")]

# (iii) Data Cleaning Action
# Remove invalid APTAIP outliers (99999)
clean_data <- my_extracted_data[my_extracted_data$Average.ping.to.attacking.IP.milliseconds != 99999,]

# Apply Log-transformations as suggested in the solution file
clean_data$log_APV <- log(clean_data$Average.ping.variability + 1)
clean_data$log_APTAIP <- log(clean_data$Average.ping.to.attacking.IP.milliseconds + 1)

# (iv) Remove incomplete cases
# Check that you still have ~90% of the data (approx 360 rows)
pca_ready_data <- na.action(clean_data)
nrow(pca_ready_data) # check the number of rows after removing incomplete cases

# (v) Perform PCA
# Use only the cleaned numeric columns (ignore original APV/APTAIP and APT)
# We sale because "Hits" (millions) and "AAPE" (samll units) have vastly different rangers

pca_final <- prcomp(pca_ready_data[, c("Hits", "Average.Request.Size.Bytes",
                                       "Attack.Window.Seconds", "Average.Attacker.Payload.Rntropy.Bits",
                                       "Individual.URLs.requested", "log_APV", "log_APTAIP")],

                                       scale = TRUE

                                       # Show the results
                                       summary(pca_final)

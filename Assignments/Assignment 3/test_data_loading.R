# Quick test - just load and clean data
library(tidyverse)
library(forcats)

cat("Loading data...\n")
dat <- read.csv("WACY-COM.csv", na.strings = c("NA", ""), stringsAsFactors = TRUE)
cat("✓ Data loaded:", nrow(dat), "rows\n")

# Step i: General Cleaning
dat <- dat %>% filter(Source.OS.Detected != "???")
dat$Source.Port.Range <- NULL
dat$IP.Range.Trust.Score <- NULL
dat <- dat %>% filter(Average.ping.to.attacking.IP.milliseconds != 99999)
dat <- dat %>% filter(Attack.Source.IP.Address.Count != -1)
cat("✓ After filtering:", nrow(dat), "rows\n")

# Step ii: Merging Categories
cat("Collapsing factors...\n")
dat$Source.OS.Detected <- fct_collapse(dat$Source.OS.Detected,
    Windows_All = c("Windows 10", "Windows Server 2008"),
    Linux_All = c("Linux (>4.0)", "Linux (2.6.3)")
)
cat("✓ Source.OS.Detected levels:", levels(dat$Source.OS.Detected), "\n")

dat$Target.Honeypot.Server.OS <- fct_collapse(dat$Target.Honeypot.Server.OS,
    Windows_All = c("Windows (Desktops)", "Windows (Servers)"),
    Unix_Like = c("Linux", "MacOS (All)")
)
cat("✓ Target.Honeypot.Server.OS levels:", levels(dat$Target.Honeypot.Server.OS), "\n")

# Step iii: Transformations
dat$log_APV <- log(dat$Average.ping.variability + 1)
dat$Average.ping.variability <- NULL
dat$sqrt_Hits <- sqrt(dat$Hits)
dat$Hits <- NULL
dat$sqrt_ASIPA <- sqrt(dat$Attack.Source.IP.Address.Count)
dat$Attack.Source.IP.Address.Count <- NULL
dat$sqrt_APTAIP <- sqrt(dat$Average.ping.to.attacking.IP.milliseconds)
dat$Average.ping.to.attacking.IP.milliseconds <- NULL
dat$sqrt_IUR <- sqrt(dat$Individual.URLs.requested)
dat$Individual.URLs.requested <- NULL
cat("✓ Transformations complete\n")

# Step iv: Remove incomplete cases
dat_cleaned <- na.omit(dat)
dat_cleaned$APT <- factor(dat_cleaned$APT, levels = c("No", "Yes"))

cat("✓ Final cleaned data:", nrow(dat_cleaned), "rows,", ncol(dat_cleaned), "cols\n")
cat("✓ APT distribution:", table(dat_cleaned$APT), "\n")

cat("\n✓✓✓ DATA CLEANING SUCCESSFUL ✓✓✓\n")

try({
  profile_path <- Sys.getenv("R_PROFILE_USER", unset = "")
  root <- if (nzchar(profile_path)) dirname(profile_path) else getwd()
  source(file.path(root, "renv/activate.R"))
}, silent = TRUE)

# ============================================================================
# R Language Studio - VS Code Configuration
# ============================================================================

# Set up options for better interactive use
options(
  max.print = 1000,
  help_type = "html",
  prompt = "> ",
  continue = "+ ",
  digits = 7,
  check.updates = FALSE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE,
  warnPartialMatchArgs = FALSE,
  verbose = FALSE,
  stringsAsFactors = FALSE
)

# Set CRAN mirror
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cran.rstudio.com"
  options(repos = r)
})

# Welcome message
.First <- function() {
  cat("\n")
  cat("╔════════════════════════════════════════════╗\n")
  cat("║  R Language Studio - VS Code Server        ║\n")
  cat("║  Machine Learning & Data Visualization Lab ║\n")
  cat("╚════════════════════════════════════════════╝\n")
  cat("R version:", getRversion(), "\n")
  cat("Platform:", Sys.info()[["sysname"]], "\n")
  cat("\nQuick Commands:\n")
  cat("  • Ctrl+Shift+Enter: Run current line\n")
  cat("  • Ctrl+Shift+S: Run selection\n")
  cat("  • Alt+-: Insert assignment operator <-\n")
  cat("  • Ctrl+Shift+M: Insert pipe operator |>\n\n")
}

# Create utility environment
.env <- new.env()

# Quick library loader
.env$lib <- function(...) {
  pkgs <- as.character(substitute(list(...)))[-1]
  for (pkg in pkgs) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}

# Package information function
.env$pkginfo <- function(pattern = NULL) {
  pkgs <- as.data.frame(installed.packages()[, c("Package", "Version", "LibPath")])
  if (!is.null(pattern)) {
    pkgs <- pkgs[grepl(pattern, pkgs$Package, ignore.case = TRUE), ]
  }
  rownames(pkgs) <- NULL
  print(pkgs)
}

# Check if package is installed
.env$is_installed <- function(pkg) {
  is.element(pkg, installed.packages()[, 1])
}

# Install if missing
.env$install_if_missing <- function(...) {
  pkgs <- as.character(substitute(list(...)))[-1]
  missing_pkgs <- pkgs[!sapply(pkgs, .env$is_installed)]
  if (length(missing_pkgs) > 0) {
    cat("Installing:", paste(missing_pkgs, collapse = ", "), "\n")
    install.packages(missing_pkgs, repos = "https://cran.rstudio.com")
  }
}

attach(.env)

# Load essential packages
suppressPackageStartupMessages({
  if (.env$is_installed("jsonlite")) library(jsonlite)
})

# Custom goodbye message
.Last.sys <- function() {
  cat("\nGoodbye! Happy coding!\n")
}

# Enable intelligent completion
utils::rc.settings(
  ipck = TRUE,
  func = TRUE,
  args = TRUE,
  fuzzy = TRUE
)

cat("✓ VS Code R Language Studio ready\n")
#### -- Consistent File Downloads -- ####
if(.Platform$OS.type == "windows") {
  options(
    download.file.method = "wininet"
  )
} else {
  options(
    download.file.method = "libcurl"
  )
}

#### -- Set CRAN -- ####
options(
  repos=c("https://cran.rstudio.com/")
)

#### -- Factors Are Not Strings -- ####
options(
  stringsAsFactors = FALSE
)

#### -- Display -- ####
options(
  digits = 12, # number of significant digits to show by default
  width = 80 # console width
)

#### -- Time Zone -- ####
if (Sys.getenv("TZ") == "") Sys.setenv("TZ" = Sys.timezone())
if (interactive()) {
  message("Session Time: ", format(Sys.time(), tz = Sys.getenv("TZ"), usetz = TRUE))
}

#### -- Session -- ####
.First <- function() {
  if (interactive()) {
    cat("\n")
    utils::timestamp("", prefix = paste("##------ [", getwd(), "] ", sep = ""))
    cat("\nSuccessfully loaded .Rprofile at", base::date(), "\n")
    cat("\n")
    cat("For more information about this project template, go to:\n")
    cat("https://github.com/startyourlab/r-project-template\n")
  }
}

if (interactive()) {
  message("Session Info: ", utils::sessionInfo()[[4]])
  message("Session User: ", Sys.info()["user"])
}

options(
  prompt = "R > ",
  continue = "... "
)

#### -- Dev Tools -- ####
# if (interactive()) {
#   library(fs)
#   library(devtools)
# }


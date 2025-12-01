# Test R Script - Environment Verification
# This script tests that R, renv, and tidyverse are working correctly

# Load tidyverse
library(tidyverse)

# Print environment info
cat("\n=== Environment Info ===\n")
cat("R Version:", R.version.string, "\n")
cat("Working Directory:", getwd(), "\n")
cat("Library Paths:", paste(.libPaths(), collapse = "\n  "), "\n")

# Print package versions
cat("\n=== Package Versions ===\n")
cat("tidyverse:", as.character(packageVersion("tidyverse")), "\n")
cat("dplyr:", as.character(packageVersion("dplyr")), "\n")
cat("ggplot2:", as.character(packageVersion("ggplot2")), "\n")

# Test basic operations
cat("\n=== Basic Operations Test ===\n")

# 1. Vector operations
vec <- c(1, 2, 3, 4, 5)
cat("Vector sum:", sum(vec), "\n")
cat("Vector mean:", mean(vec), "\n")

# 2. Data frame creation
test_df <- tibble(
  id = 1:5,
  name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
  score = c(85, 92, 78, 95, 88),
  passed = score >= 80
)

cat("\n=== Test Data Frame ===\n")
print(test_df)

# 3. dplyr operations
cat("\n=== dplyr Test ===\n")
summary_stats <- test_df %>%
  summarise(
    count = n(),
    avg_score = mean(score),
    max_score = max(score),
    pass_rate = sum(passed) / n() * 100
  )
print(summary_stats)

# 4. ggplot2 test (create simple plot)
cat("\n=== Creating test plot ===\n")
p <- ggplot(test_df, aes(x = name, y = score, fill = passed)) +
  geom_col() +
  labs(
    title = "Test Scores",
    x = "Student",
    y = "Score"
  ) +
  theme_minimal()

# Save plot
ggsave("test_plot.png", plot = p, width = 8, height = 6)
cat("Plot saved as: test_plot.png\n")

# 5. String operations
cat("\n=== stringr Test ===\n")
test_strings <- c("apple", "banana", "cherry")
upper_strings <- str_to_upper(test_strings)
cat("Original:", paste(test_strings, collapse = ", "), "\n")
cat("Uppercase:", paste(upper_strings, collapse = ", "), "\n")

cat("\n=== All Tests Completed Successfully! ===\n")

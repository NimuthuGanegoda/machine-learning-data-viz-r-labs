# Visualization Demo using Base R (Week 6)
# Generates a few common charts and saves them to PNG files

# library(tidyverse) # Removed dependency

# 1) Scatter plot with regression line
set.seed(1)
scatter_df <- data.frame(
  x = rnorm(200, mean = 50, sd = 10),
  group = sample(c("A", "B"), 200, replace = TRUE)
)
scatter_df$y <- 0.8 * scatter_df$x + rnorm(200, sd = 8)

png("viz_scatter.png", width = 800, height = 600)
# Create color vector
colors <- ifelse(scatter_df$group == "A", "red", "blue")
plot(scatter_df$x, scatter_df$y, col = colors, pch = 19,
     main = "Scatter with Trend", xlab = "X", ylab = "Y")
# Add legend
legend("topleft", legend = c("A", "B"), col = c("red", "blue"), pch = 19)
# Add trend lines
abline(lm(y ~ x, data = scatter_df[scatter_df$group == "A",]), col = "red", lwd = 2)
abline(lm(y ~ x, data = scatter_df[scatter_df$group == "B",]), col = "blue", lwd = 2)
dev.off()

# 2) Bar chart (categorical counts)
bar_df <- data.frame(
  category = sample(LETTERS[1:6], 300, replace = TRUE)
)
counts <- table(bar_df$category)

png("viz_bar.png", width = 800, height = 600)
barplot(counts, col = rainbow(length(counts)),
        main = "Category Counts", xlab = "Category", ylab = "Count")
dev.off()

# 3) Line chart (time series)
line_df <- data.frame(
  date = seq.Date(as.Date("2025-01-01"), by = "day", length.out = 120),
  value = cumsum(rnorm(120, mean = 0.2, sd = 1)) + 20
)

png("viz_line.png", width = 900, height = 500)
plot(line_df$date, line_df$value, type = "l", col = "steelblue", lwd = 2,
     main = "Daily Metric", xlab = "Date", ylab = "Value")
# Loess smooth
loess_fit <- loess(value ~ as.numeric(date), data = line_df)
lines(line_df$date, predict(loess_fit), col = "tomato", lwd = 2)
dev.off()

# 4) Faceted small multiples
t_vec <- rep(1:60, times = 4)
facet_df <- data.frame(
  grp = rep(c("North", "South", "East", "West"), each = 60),
  t = t_vec,
  metric = 10 + rep(c(2, -1, 0.5, 1.2), each = 60) * t_vec + rnorm(240, sd = 5)
)

png("viz_facets.png", width = 1000, height = 600)
par(mfrow = c(2, 2))
groups <- unique(facet_df$grp)
for(g in groups) {
  sub_df <- facet_df[facet_df$grp == g,]
  plot(sub_df$t, sub_df$metric, type = "l", main = g, xlab = "Time", ylab = "Metric")
}
dev.off()

cat("Saved plots: viz_scatter.png, viz_bar.png, viz_line.png, viz_facets.png\n")

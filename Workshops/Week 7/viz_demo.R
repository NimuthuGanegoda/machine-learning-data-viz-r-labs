# Visualization Demo using tidyverse (Week 7)
# Generates a few common charts and saves them to PNG files

library(tidyverse)

# 1) Scatter plot with regression line
set.seed(1)
scatter_df <- tibble(
  x = rnorm(200, mean = 50, sd = 10),
  y = 0.8 * x + rnorm(200, sd = 8),
  group = sample(c("A", "B"), 200, replace = TRUE)
)

p_scatter <- ggplot(scatter_df, aes(x = x, y = y, color = group)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter with Trend", x = "X", y = "Y") +
  theme_minimal()

ggsave("viz_scatter.png", p_scatter, width = 8, height = 6)

# 2) Bar chart (categorical counts)
bar_df <- tibble(
  category = sample(LETTERS[1:6], 300, replace = TRUE)
) %>%
  count(category)

p_bar <- ggplot(bar_df, aes(x = category, y = n, fill = category)) +
  geom_col() +
  labs(title = "Category Counts", x = "Category", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("viz_bar.png", p_bar, width = 8, height = 6)

# 3) Line chart (time series)
line_df <- tibble(
  date = seq.Date(as.Date("2025-01-01"), by = "day", length.out = 120),
  value = cumsum(rnorm(120, mean = 0.2, sd = 1)) + 20
)

p_line <- ggplot(line_df, aes(x = date, y = value)) +
  geom_line(color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "tomato") +
  labs(title = "Daily Metric", x = "Date", y = "Value") +
  theme_minimal()

ggsave("viz_line.png", p_line, width = 9, height = 5)

# 4) Faceted small multiples
facet_df <- tibble(
  grp = rep(c("North", "South", "East", "West"), each = 60),
  t = rep(1:60, times = 4),
  metric = 10 + rep(c(2, -1, 0.5, 1.2), each = 60) * t + rnorm(240, sd = 5)
)

p_facet <- ggplot(facet_df, aes(x = t, y = metric)) +
  geom_line() +
  facet_wrap(~ grp, ncol = 2) +
  labs(title = "Regional Trends", x = "Time", y = "Metric") +
  theme_minimal()

ggsave("viz_facets.png", p_facet, width = 10, height = 6)

cat("Saved plots: viz_scatter.png, viz_bar.png, viz_line.png, viz_facets.png\n")

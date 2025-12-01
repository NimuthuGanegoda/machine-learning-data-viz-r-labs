Visualization Preview
================

- [Scatter with Trend](#scatter-with-trend)
- [Category Counts (Bar)](#category-counts-bar)
- [Time Series (Line)](#time-series-line)
- [Faceted Small Multiples](#faceted-small-multiples)

# Scatter with Trend

``` r
set.seed(1)
scatter_df <- tibble(
  x = rnorm(200, mean = 50, sd = 10),
  y = 0.8 * x + rnorm(200, sd = 8),
  group = sample(c("A", "B"), 200, replace = TRUE)
)

ggplot(scatter_df, aes(x = x, y = y, color = group)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter with Trend", x = "X", y = "Y") +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](visualizations_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

# Category Counts (Bar)

``` r
bar_df <- tibble(
  category = sample(LETTERS[1:6], 300, replace = TRUE)
) %>%
  count(category)

ggplot(bar_df, aes(x = category, y = n, fill = category)) +
  geom_col() +
  labs(title = "Category Counts", x = "Category", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")
```

![](visualizations_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# Time Series (Line)

``` r
line_df <- tibble(
  date = seq.Date(as.Date("2025-01-01"), by = "day", length.out = 120),
  value = cumsum(rnorm(120, mean = 0.2, sd = 1)) + 20
)

ggplot(line_df, aes(x = date, y = value)) +
  geom_line(color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "tomato") +
  labs(title = "Daily Metric", x = "Date", y = "Value") +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](visualizations_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

# Faceted Small Multiples

``` r
facet_df <- tibble(
  grp = rep(c("North", "South", "East", "West"), each = 60),
  t = rep(1:60, times = 4),
  metric = 10 + rep(c(2, -1, 0.5, 1.2), each = 60) * t + rnorm(240, sd = 5)
)

ggplot(facet_df, aes(x = t, y = metric)) +
  geom_line() +
  facet_wrap(~ grp, ncol = 2) +
  labs(title = "Regional Trends", x = "Time", y = "Metric") +
  theme_minimal()
```

![](visualizations_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

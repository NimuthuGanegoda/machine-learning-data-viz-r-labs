#!/usr/bin/env Rscript
# Generate all visualizations quickly
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Load packages
suppressPackageStartupMessages({
  library(ggplot2)
  library(scatterplot3d)
  library(scales)
})

cat("=== Generating Visualizations ===\n\n")

## Week 3 - PCA Visualizations
cat("Week 3: PCA Analysis...\n")
setwd("Workshops/Week 3")
data(iris)
iris$Species <- factor(iris$Species, labels = c("Setosa", "Versicolor", "Virginica"))
pca.iris <- prcomp(iris[, 1:4], scale = TRUE)

# 3D Plot
png("pca_3d_iris.png", width = 1200, height = 900, res = 120)
scatterplot3d(pca.iris$x[, 1:3],
  pch = 21, color = as.numeric(iris$Species) + 1,
  bg = alpha(as.numeric(iris$Species) + 1, 0.4),
  cex.symbols = 2, col.grid = "steelblue", col.axis = "steelblue",
  angle = 70, main = "3D PCA - Iris Dataset",
  xlab = "PC1", ylab = "PC2", zlab = "PC3"
)
legend("topright",
  legend = c("Setosa", "Versicolor", "Virginica"),
  col = c(2, 3, 4), pch = 21, pt.bg = alpha(c(2, 3, 4), 0.4), pt.cex = 2
)
dev.off()
cat("  ✓ pca_3d_iris.png\n")

# 2D Plot
png("pca_2d_iris.png", width = 1000, height = 800, res = 120)
df <- data.frame(pca.iris$x, Species = iris$Species)
print(ggplot(df, aes(x = PC1, y = PC2, color = Species)) +
  geom_point(alpha = 0.8, size = 4) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  ggtitle("2D PCA - Iris Dataset"))
dev.off()
cat("  ✓ pca_2d_iris.png\n")

setwd("../..")

## Week 2 - Scatter Plots
cat("Week 2: Data Visualization...\n")
setwd("Workshops/Week 2")
set.seed(123)
Salary_Data <- data.frame(
  YearsExperience = seq(1, 10.5, length.out = 30),
  Salary = c(
    39343, 46205, 37731, 43525, 39891, 56642, 60150, 54445,
    64445, 57189, 63218, 55794, 56957, 57081, 61111, 67938,
    66029, 83088, 81363, 93940, 91738, 98273, 101302, 113812,
    109431, 105582, 116969, 112635, 122391, 121872
  )
)

png("salary_scatter.png", width = 1000, height = 800, res = 120)
print(ggplot(Salary_Data, aes(x = YearsExperience, y = Salary)) +
  geom_point(color = "steelblue", size = 4, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal(base_size = 14) +
  ggtitle("Salary vs Years of Experience") +
  xlab("Years of Experience") +
  ylab("Salary ($)"))
dev.off()
cat("  ✓ salary_scatter.png\n")

setwd("../..")

## Week 4 - Hierarchical Clustering
cat("Week 4: Hierarchical Clustering...\n")
setwd("Workshops/Week 4")
data(mtcars)
dist_matrix <- dist(scale(mtcars))
hc <- hclust(dist_matrix, method = "ward.D2")

png("dendrogram.png", width = 1200, height = 800, res = 120)
plot(hc,
  main = "Hierarchical Clustering - mtcars Dataset",
  xlab = "Cars", ylab = "Height", cex = 0.8
)
rect.hclust(hc, k = 3, border = "red")
dev.off()
cat("  ✓ dendrogram.png\n")

setwd("../..")

cat("\n✓ All visualizations generated successfully!\n")
cat("\nGenerated files:\n")
cat("  - Workshops/Week 3/pca_3d_iris.png\n")
cat("  - Workshops/Week 3/pca_2d_iris.png\n")
cat("  - Workshops/Week 3/pca_3d_plot.png (from earlier)\n")
cat("  - Workshops/Week 2/salary_scatter.png\n")
cat("  - Workshops/Week 4/dendrogram.png\n")

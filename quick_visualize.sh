#!/bin/bash
# Quick visualization generator for all R scripts

cd "/home/nimuthu/Repo/machine-learning-data-viz-r-labs"

echo "=== Quick Visualization Generator ==="
echo ""

# Week 3 - PCA/PCoA
echo "Generating Week 3 visualizations..."
cd "Workshops/Week 3"
R --quiet --no-save --no-restore << 'EOF'
library(scales, quietly=TRUE)
library(scatterplot3d, quietly=TRUE)
library(ggplot2, quietly=TRUE)

# PCA 3D Plot
data(iris)
iris$Species <- factor(iris$Species, labels = c("Setosa", "Versicolor", "Virginica"))
pca.iris <- prcomp(iris[, 1:4], scale = TRUE)

png('pca_3d_iris.png', width=1200, height=900, res=120)
scatterplot3d(pca.iris$x[, 1:3], pch=21, color=as.numeric(iris$Species)+1,
  bg=alpha(as.numeric(iris$Species)+1, 0.4), cex.symbols=2,
  col.grid='steelblue', col.axis='steelblue', angle=70,
  main='3D PCA - Iris Dataset', xlab='PC1', ylab='PC2', zlab='PC3')
legend('topright', legend=c('Setosa','Versicolor','Virginica'),
  col=c(2,3,4), pch=21, pt.bg=alpha(c(2,3,4),0.4), pt.cex=2, cex=0.8)
dev.off()

# PCA 2D Plot
png('pca_2d_iris.png', width=1000, height=800, res=120)
df <- data.frame(pca.iris$x, Species=iris$Species)
print(ggplot(df, aes(x=PC1, y=PC2)) +
  geom_point(aes(colour=Species), alpha=0.8, size=4) +
  theme_minimal(base_size=14) + theme(legend.position='top') +
  xlab('PC1') + ylab('PC2') + ggtitle('2D PCA - Iris Dataset'))
dev.off()

cat('✓ Week 3 visualizations saved\n')
EOF

cd ../..

# Week 2 - GGPlot visualizations
echo "Generating Week 2 visualizations..."
cd "Workshops/Week 2"
R --quiet --no-save --no-restore << 'EOF'
library(ggplot2, quietly=TRUE)

# Generate sample salary data
set.seed(123)
Salary_Data <- data.frame(
  YearsExperience = c(1.1, 1.3, 1.5, 2.0, 2.2, 2.9, 3.0, 3.2, 3.2, 3.7,
                      3.9, 4.0, 4.0, 4.1, 4.5, 4.9, 5.1, 5.3, 5.9, 6.0,
                      6.8, 7.1, 7.9, 8.2, 8.7, 9.0, 9.5, 9.6, 10.3, 10.5),
  Salary = c(39343, 46205, 37731, 43525, 39891, 56642, 60150, 54445,
             64445, 57189, 63218, 55794, 56957, 57081, 61111, 67938,
             66029, 83088, 81363, 93940, 91738, 98273, 101302, 113812,
             109431, 105582, 116969, 112635, 122391, 121872)
)

png('salary_scatter.png', width=1000, height=800, res=120)
print(ggplot(Salary_Data, aes(x=YearsExperience, y=Salary)) +
  geom_point(color='steelblue', size=4, alpha=0.7) +
  geom_smooth(method='lm', color='red', se=TRUE) +
  theme_minimal(base_size=14) +
  ggtitle('Salary vs Years of Experience') +
  xlab('Years of Experience') + ylab('Salary ($)'))
dev.off()

cat('✓ Week 2 visualizations saved\n')
EOF

cd ../..

# Week 4 - Hierarchical Clustering
echo "Generating Week 4 visualizations..."
cd "Workshops/Week 4"
R --quiet --no-save --no-restore << 'EOF'
library(ggplot2, quietly=TRUE)

# Use built-in dataset
data(mtcars)

# Hierarchical clustering
dist_matrix <- dist(scale(mtcars))
hc <- hclust(dist_matrix, method='ward.D2')

png('dendrogram.png', width=1200, height=800, res=120)
plot(hc, main='Hierarchical Clustering - mtcars Dataset',
     xlab='Cars', ylab='Height', cex=0.8)
rect.hclust(hc, k=3, border='red')
dev.off()

cat('✓ Week 4 visualizations saved\n')
EOF

cd ../..

echo ""
echo "=== Visualization Generation Complete ==="
echo ""
echo "Generated files:"
find Workshops -name "*.png" -type f -newer quick_visualize.sh 2>/dev/null

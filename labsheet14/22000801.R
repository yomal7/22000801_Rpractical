# Dataset Exploration

data(iris)

str(iris)

summary(iris)

head(iris)

num_of_species <- length(unique(iris$Species))
print(paste("Number of species in the dataset : ", num_of_species))
cat("Species names:", paste(unique(iris$Species), collapse=", "), "\n")



#remove any rows contain NA
if(sum(is.na(iris)) > 0){
  iris <- na.omit(iris)
}


cat("Mean values:\n")
print(paste("Sepal Length : ", mean(iris$Sepal.Length)))
print(paste("Sepal Width : ", mean(iris$Sepal.Width)))
print(paste("Petal Length : ", mean(iris$Petal.Length)))
print(paste("Petal Width : ", mean(iris$Petal.Width)))

cat("\nMedian values:\n")
print(paste("Sepal Length : ", median(iris$Sepal.Length)))
print(paste("Sepal Width : ", median(iris$Sepal.Width)))
print(paste("Petal Length : ", median(iris$Petal.Length)))
print(paste("Petal Width : ", median(iris$Petal.Width)))

cat("\nStandard deviation values:\n")
print(paste("Sepal Length : ", sd(iris$Sepal.Length)))
print(paste("Sepal Width : ", sd(iris$Sepal.Width)))
print(paste("Petal Length : ", sd(iris$Petal.Length)))
print(paste("Petal Width : ", sd(iris$Petal.Width)))

# Data Visualization

species_counts <- table(iris$Species)
species_percent <- round((species_counts/sum(species_counts))*100, 1)
labels <- paste(names(species_counts), " (", species_percent, "%)", sep="")


png("species_pie_chart.png", width=800, height=600)
pie(species_counts, labels=labels, 
    main="Distribution of Iris Species", 
    col=rainbow(length(species_counts)))
dev.off()

png("species_bar_chart.png", width=800, height=600)
barplot(species_counts, 
        main="Count of Iris Species", 
        xlab="Species", ylab="Count", 
        col=rainbow(length(species_counts)))
dev.off()

png("sepal_length_histogram.png", width=800, height=600)
hist(iris$Sepal.Length, main="Histogram of Sepal Length", 
     xlab="Sepal Length (cm)", 
     col="lightblue", breaks=10)
dev.off()

png("petal_length_histogram.png", width=800, height=600)
hist(iris$Petal.Length, main="Histogram of Petal Length", 
     xlab="Sepal Length (cm)", 
     col="lightgreen", breaks=10)
dev.off()

png("sepal_petal_scatterplot.png", width=800, height=600)
plot(iris$Sepal.Length, iris$Petal.Length, 
     main="Scatterplot: Sepal Length vs Petal Length",
     xlab="Sepal Length (cm)", ylab="Petal Length (cm)",
     col=as.numeric(iris$Species), pch=16)
legend("topleft", legend=levels(iris$Species), col=1:3, pch=16)

abline(lm(Petal.Length ~ Sepal.Length, data=iris), col="red")

correlation <- cor(iris$Sepal.Length, iris$Petal.Length)
text(7, 2, paste("Correlation =", round(correlation, 3)), pos=4)
dev.off()

# Hypothesis Testing

alpha <- 0.05

cat("\n\n1. Lower Tail Test: Is the average Sepal Length significantly lower than 5.8 cm?\n")

cat("Null Hypothesis (H0): mu >= 5.8 cm\n")
cat("Alternative Hypothesis (H1): mu < 5.8 cm\n\n")

t_test_lower <- t.test(iris$Sepal.Length, mu=5.8, alternative="less")
print(t_test_lower)

if(t_test_lower$p.value < alpha) {
  cat("\nConclusion: Reject H0. There is sufficient evidence to conclude that the average Sepal 
      Length is significantly lower than 5.8 cm.\n")
} else {
  cat("\nConclusion: Fail to reject H0. There is insufficient evidence to conclude that the 
      average Sepal Length is significantly lower than 5.8 cm.\n")
}

cat("\n\n2. Upper Tail Test: Is the average Petal Length significantly greater than 3.5 cm?\n")

cat("Null Hypothesis (H0): mu <= 3.5 cm\n")
cat("Alternative Hypothesis (H1): mu > 3.5 cm\n\n")

t_test_upper <- t.test(iris$Petal.Length, mu=3.5, alternative="greater")
print(t_test_upper)

if(t_test_upper$p.value < alpha) {
  cat("\nConclusion: Reject H0. There is sufficient evidence to conclude that the average 
      Petal Length is significantly greater than 3.5 cm.\n")
} else {
  cat("\nConclusion: Fail to reject H0. There is insufficient evidence to conclude 
      that the average Petal Length is significantly greater than 3.5 cm.\n")
}

cat("\n\n3. Two-Tailed Test: Is the average Sepal Width significantly different from 3.0 cm?\n")

cat("Null Hypothesis (H0): mu = 3.0 cm\n")
cat("Alternative Hypothesis (H1): mu ≠ 3.0 cm\n\n")

t_test_two_tailed <- t.test(iris$Sepal.Width, mu=3.0, alternative="two.sided")
print(t_test_two_tailed)


if(t_test_two_tailed$p.value < alpha) {
  cat("\nConclusion: Reject H0. There is sufficient evidence to conclude that the 
      average Sepal Width is significantly different from 3.0 cm.\n")
} else {
  cat("\nConclusion: Fail to reject H0. There is insufficient evidence to conclude 
      that the average Sepal Width is significantly different from 3.0 cm.\n")
}

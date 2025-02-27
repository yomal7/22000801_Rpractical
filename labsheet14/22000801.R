#Dataset Exploration
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


#Data Visualization
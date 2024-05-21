#Load Libraries

# Install necessary libraries
install.packages("caret")
install.packages("ggplot2")
install.packages("e1071") 

# Load the libraries
library(caret)
library(ggplot2)

# Load the Iris dataset
data(iris)

head(iris)

# Summary statistics of the dataset
summary(iris)

#structure of the dataset
str(iris)

# Visualize the Data
pairs(iris, col = iris$Species)

# Sepal dimensions visualization
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +geom_point() +theme_minimal() +labs(title = "Sepal Length vs Sepal Width")

# Petal dimensions visualization
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +geom_point() +theme_minimal() +labs(title = "Petal Length vs Petal Width")


# Split the Data into Training and Testing Sets

set.seed(123)

# Split the data
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
irisTrain <- iris[trainIndex, ]
irisTest <- iris[-trainIndex, ]

# Check the dimensions of the splits
dim(irisTrain)
dim(irisTest)

# Train a Random Forest model
model <- train(Species ~ ., data = irisTrain, method = "rf", trControl = trainControl(method = "cv", number = 10))

# Print the trained model
print(model)

#Evaluate the Model

# Make predictions on the test set
predictions <- predict(model, newdata = irisTest)

#confusion matrix
confMatrix <- confusionMatrix(predictions, irisTest$Species)
print(confMatrix)

#accuracy
cat("Accuracy: ", confMatrix$overall['Accuracy'], "\n")

# Example new data for prediction
new_data <- data.frame(Sepal.Length = c(5.1, 6.2),Sepal.Width = c(3.5, 3.4),Petal.Length = c(1.4, 5.4),Petal.Width = c(0.2, 2.3))

new_predictions <- predict(model, new_data)
print(new_predictions)


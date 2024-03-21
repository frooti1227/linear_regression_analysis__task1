# Load libraries
library(readr)
library(ggplot2)

# Read the data
data <- read.csv("http://bit.ly/w-data")

# Explore the data
head(data)

# Check summary statistics
summary(data)

# Plot the data
ggplot(data, aes(x = Hours, y = Scores)) +
  geom_point() +
  ggtitle("Hours Studied vs Scores Obtained") +
  xlab("Hours Studied") +
  ylab("Scores Obtained")

# Split the data into training and testing sets
set.seed(123)
trainIndex <- sample(1:nrow(data), 0.8 * nrow(data))
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# linear regression model
lm_model <- lm(Scores ~ Hours, data = trainData)

# Summary of the model
summary(lm_model)

# Predict the scores for test data
testData$predicted_scores <- predict(lm_model, newdata = testData)

# Visualize actual vs predicted scores for test data
ggplot(testData, aes(x = Hours, y = Scores)) +
  geom_point(color = "blue") +
  geom_line(aes(y = predicted_scores), color = "black") +
  ggtitle("Actual vs Predicted Scores") +
  xlab("Hours Studied") +
  ylab("Scores")

# Predict the score for a student studying 9.25 hours/day
my_data <- data.frame(Hours = 9.25)
predicted_score <- predict(lm_model, newdata = my_data)
print("If student studies for 9.5 hours, the predicted score is")
predicted_score

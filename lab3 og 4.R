data <- read.csv("C:/Users/mikkel/Documents/Rapport 3 Data Science/BreastCancer.csv")

# Questiread.csv()# Question 2: How many samples are in the data set? How many different tumor features are in the data set?
num_samples <- nrow(data)
num_features <- ncol(data) - 2  # Excluding 'id' and 'diagnosis' columns
question_2 <- paste("There are", num_samples, "samples in the data set and", num_features, "different tumor features.")
# Answer 2: There are 569 different samples, and 31 tumours

# Question 3: Which feature do you consider to be the one that should be predicted? And why?
# Answer 3: The feature to be predicted is 'diagnosis' because it indicates whether the tumor is benign (B) or malignant (M)

# Question 4: What is the class distribution of the data set? How many samples are benign and how many are malignant?
class_distribution <- table(data$diagnosis)
benign_count <- class_distribution['B']
malignant_count <- class_distribution['M']
#The class distribution of the data set is", benign_count, "benign samples and", malignant_count, "malignant samples.
benign_count
malignant_count
# Answer 4: There are 357 benign samples and 212 malignant samples

# Question 5: Which feature has the highest mean value? Which feature has the lowest mean value?
numeric_features <- data[, -c(1, 2)]
means <- colMeans(numeric_features)
highest_mean_feature <- names(means)[which.max(means)]
lowest_mean_feature <- names(means)[which.min(means)]
# Print answers
print(highest_mean_feature)
print(lowest_mean_feature)
# Answer 5: The highest mean is "area_worst" and the lowest mean is "fractal_dimension_se"

# Question 6: Which feature has the highest standard deviation? Which feature has the lowest standard deviation?
std_devs <- apply(numeric_features, 2, sd)
highest_std_feature <- names(std_devs)[which.max(std_devs)]
lowest_std_feature <- names(std_devs)[which.min(std_devs)]
# Print answers
print(highest_std_feature)
print(lowest_std_feature)
# Answer 6: The highest standard deviation is "area_worst" and the lowest is "fractal_dimension_se"

# Question 7: How would one make these differences more equivalent to avoid that some features become more influential than others?
#Answer 7: One would normalize or standardize the features to make these differences more equivalent
#Normalization scales the features to a range of [0, 1], while standardization scales them to have a mean of 0 and a standard deviation of 1.

# Question 8: What is the Pearson correlation? What is the range of values?
# Answer 8: The Pearson correlation measures the linear relationship between two variables.
#The range of values is from -1 to +1, where -1 indicates a perfect negative linear relationship.
#+1 indicates a perfect positive linear relationship, and 0 indicates no linear relationship.

# Question 9: Which feature pair is most correlated? Which feature pair is least correlated?
correlation_matrix <- cor(numeric_features)

# Convert the correlation matrix to a data frame
correlation_pairs <- as.data.frame(as.table(correlation_matrix))

# Rename the columns for clarity
colnames(correlation_pairs) <- c("Feature1", "Feature2", "Correlation")


# Remove rows where a feature is correlated with itself (Correlation = 1)
correlation_pairs <- correlation_pairs[correlation_pairs$Correlation != 1, ]

# Remove duplicate pairs (e.g., avoid having both A-B and B-A)
upper_triangle_indices <- upper.tri(correlation_matrix)
upper_triangle_pairs <- correlation_pairs[upper_triangle_indices[as.matrix(expand.grid(1:ncol(correlation_matrix), 1:ncol(correlation_matrix)))], ]

# Sort by absolute correlation in descending order
sorted_correlation_pairs <- upper_triangle_pairs[order(-abs(upper_triangle_pairs$Correlation)), ]

# Get the most correlated pair
most_correlated_pair <- sorted_correlation_pairs[1, ]

# Sort by absolute correlation in ascending order to get the least correlated pair
least_correlated_pairs <- upper_triangle_pairs[order(abs(upper_triangle_pairs$Correlation)), ]
least_correlated_pair <- least_correlated_pairs[1, ]

# Print the results
print("Most Correlated Feature Pair:")
print(most_correlated_pair)
print("\nLeast Correlated Feature Pair:")
print(least_correlated_pair)

# Answer 9: The pair with the highest correlation is perimeter_worst and radius worst, and the lowest is radius_se and fractal_dimension_mean.

# Question 10: When you get a negative value for the correlation, what does that mean?
# Answer 10: It means that there is a negativ correlation, meaning that as one feature increases, the other decreases.
# An inverse relationship

# Load ggplot
library(ggplot2)

# Question 11: Are these features potentially valuable indicators for the diagnosis of a tumor?
ggplot(data, aes(x = radius_mean, y = texture_mean, color = diagnosis)) +
  geom_point() +
  labs(title = "Scatter plot of radius_mean vs texture_mean. First two features", x = "radius_mean", y = "texture_mean") +
  scale_color_manual(values = c("B" = "skyblue", "M" = "tomato"))

# Answer 11: The scatter plot shows a separation between the benign and malignant tumours, based on the first two features.
# This could be a way of determining a diagnosis, since there seems to be a clear seperation of the data.

# Question 12: How similar are the first two features? How does the figure change when you plot the most and least correlated features?
ggplot(data, aes(x = radius_se, y = fractal_dimension_mean, color = diagnosis)) +
  geom_point() +
  labs(title = "Scatter plot of radius_se vs fractal_dimension_mean. Least correlated", x = "radius_se", y = "fractal_dimension_mean") +
  scale_color_manual(values = c("B" = "skyblue", "M" = "tomato"))

ggplot(data, aes(x = perimeter_worst, y = radius_worst, color = diagnosis)) +
  geom_point() +
  labs(title = "Scatter plot of perimeter_worst vs radius_worst. Most correlated", x = "perimeter_worst", y = "radius_worst") +
  scale_color_manual(values = c("B" = "skyblue", "M" = "tomato"))


# Question 13: Which features are potentially more useful for classification?
#Highly correlated features or features that are not correlated?
# Answer 13: Features that have a higher correlation are more useful for classification, because they can provide a higher relationship between the features

sum(is.na(numeric_features))         # how many NA values
sum(is.nan(as.matrix(numeric_features)))  # how many NaN values
sum(is.infinite(as.matrix(numeric_features)))  # how many Inf values

numeric_features_clean <- data[, sapply(data, is.numeric)]

numeric_features_clean_cleaned <- na.omit(numeric_features_clean)

str(numeric_features_clean_cleaned)


numeric_matrix <- as.matrix(numeric_features_clean_cleaned)
is.numeric(numeric_matrix)


# Plot the heatmap
heatmap(numeric_matrix, Rowv=NA, Colv=NA, scale="column",
        margins=c(5,10), RowSideColors = rainbow(2)[as.factor(data$diagnosis)],
        main="Heatmap of the Breast Cancer data set", xlab="", ylab="Samples")
legend("bottomright", fill=rainbow(2), legend=c("malign", "benign"))

mtext("Features", side = 1, line = 7)  # Adjust 'line' to move the label down


# Question 14: Which part of the figure tells which tumor diagnosis is more likely?
# Answer 14: The part of the figure where colors change abruptly indicates which tumor diagnosis is more likely.

# Now, change the arguments Rowv and Colv to TRUE and rerun the code.
heatmap(numeric_matrix, Rowv=TRUE, Colv=TRUE, scale="column",
        margins=c(5,10), RowSideColors = rainbow(2)[as.factor(data$diagnosis)],
        main="Heatmap of the Breast Cancer data set", xlab="", ylab="Samples")
legend("bottomright", fill=rainbow(2), legend=c("malign", "benign"))

mtext("Features", side = 1, line = 7)  # Adjust 'line' to move the label down

# Question 15: What is the difference between the two figures?
# Answer 15: The difference between the two figures is that in the reordered heatmap, similar features and samples are grouped together, making patterns more visible.

# Question 16: When ordering samples and features according to their similarity, do you get a better separation of the two classes? Why would that be?
# Answer 16: "When ordering samples and features according to their similarity, you get a better separation of the two classes because similar samples and features are grouped together.
# Highlighting differences between benign and malignant tumors.

# Calculate the principal components
num_data <- data[,3:32]
my_pca <- prcomp(num_data, center=TRUE, scale=TRUE)
# Summary
summary(my_pca)

# Visualize the dataset when plotted on first two principal components
# Making a data frame for plotting
pca_df <- data.frame(PC1 = my_pca$x[,1],
                     PC2 = my_pca$x[,2],
                     diagnosis = data$diagnosis)
# Plot using ggplot2
ggplot(pca_df, aes(x = PC1, y = PC2, color = diagnosis)) +
  geom_point(alpha = 0.7, size = 2.5) +
  labs(title = "PCA of Breast Cancer Data",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  scale_color_manual(values = c("B" = "skyblue", "M" = "tomato"))

# Question 17: How much variance is captured in the first two principal components?
summary(my_pca)
# PC1 has 0.4427 proportion of variance and PC2 has 0.1897 variance. Combined they make up 63.24% of total variance.

#Question 18: How well are the two classes separated?
#Answer 18: The two classes show a clear seperation of malignant and benign diagnoses, which show a strong separation.

#Question 19: Why do we usually run a principal component analysis (PCA) on a high-dimensional data set like this one?
# Answer 19: A data set with a lot of variables would be impossible to plot on a 2D or 3D plot, requiring more dimensions to visualize it.
#PCA makes it possible by condensing the information and plotting the greatest variance, and the next greatest and so on, creating a linear line between points making it possible to plot.

#Question 20: Which biological reason could there be for the large spread of the malign tumors?
# Malignant tumors have a greater variability in cell structure, size and behavior. With a great mutation rate, they have the ability to show a wider range of characteristics, leading to a greater spread.

# Classification

#Convert the diagnoses column to a factor
data$diagnosis <- as.factor(data$diagnosis)

# Selecting numeric features
data_model <- data[, c(2,3:32)]

# Use logistic regression model
log_model <- glm(diagnosis ~ ., data = data_model, family = "binomial")

# Summary
summary(log_model)

# Question 21: What do you observe when running the logisitc regression? Are there errors or warnings?
# Two warnings appeared
# 1. glm.fit: algorithm did not converge
# 2. glm.fit: fitted probabilities numerically 0 or 1 occurred

# Removing highly correlated features
library(tidyverse)
library(caret)

# The plus two is needed since the first two columns were non-numerical
features_to_remove = findCorrelation(cor(num_data), cutoff = 0.9) + 2

data_train = data[,-features_to_remove]

#Discard the ID and X column
data_train = data_train[,-c(1,23)]

# Use logistic regression model again
log_model2 <- glm(diagnosis ~ ., data = data_train, family = "binomial")

# Summary
summary(log_model)

# Question 22: Which features are significant (assuming a significance level of <0.05)?
# Every feature except for concavity_mean is significant.

# Question 23: What is the interpretation of the coefficients?
# The coefficients show the effect of an increase in the feature related to the likelyhood of the tumor being malignant
# A positive coefficient increases likelyhood of tumor being malignant and a negative increases the likelyhood.

# Question 24: What is the interpretation of the intercepts?
# Answer 24: The intercept shows the baseline odds of the tumor being malignant when all other predictors are zero

# Question 25: What is your accuracy on the training data?
model_train <- train(
  diagnosis ~ .,
  data = data_train,
  method = "glm",
  trControl = trainControl(method = "none")  # No CV = train on full data
)

# Get predictions on the training data
pred_train <- predict(model_train, newdata = data_train)

# Calculate training accuracy
confusionMatrix(pred_train, data_train$diagnosis)$overall["Accuracy"]

# Print accuracy
print(0.9876977)

# Task VIII

train.control <- trainControl(method = "cv", number = 10)
model <- train(diagnosis ~., data = data_train, method = "glm", trControl = train.control)
print(model)

# Question 26: What is the average accuracy you achieve?
print(0.959553)

# Question 27: Compare the accuracy from the task before. Are the differences expected?
# The drop in accuracy is expected as training accuracy often overestimates accuracy, and using cross validation often gives a slightly lower but more accurate accuracy.

# Task IX
library(randomForest)

# Question 28: What performance does the randomForest achieve as OOB error rate?
# Fit random forest
rf_model <- randomForest(diagnosis ~ ., data = data_train, importance = TRUE)
# Print
print(rf_model)
# Print error rate
print("4.39%")

# Question 29: How well does it perform on the entire dataset? Can you argue why the OOB is worse then the overall classification?
# Predict entire dataset:
rf_preds <- predict(rf_model, newdata = data_train)

# Compare
confusionMatrix(rf_preds, data_train$diagnosis)
# Answer 29: The OOB error is slightly higher than the overall training accuracy because it provides a more realistic estimate of the model's performance on unseen data.
# While training accuracy reflects how well the model fits the data it was trained on, OOB error uses only the trees that did not see each sample during training.
# This built-in validation approach helps avoid overfitting and gives a better sense of how the model will generalize.

# Question 30: Plot and compare the most important features. Compare with the results for the linear regression.
varImpPlot(rf_model)
# Features such as concave.points_worst, radius_mean and area_worst largely overlap with those identified in the logistic regression.
# This suggests that these features a indicators of tumor malignancy.
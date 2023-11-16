# Load necessary libraries
library(readxl)

# Read the data (adjust the file path as necessary)
train_data <- read_excel("IS71-credit defaulter case study.xlsx", sheet = "training_data")
test_data <- read_excel("IS71-credit defaulter case study.xlsx", sheet = "testing_data")

# Data Cleaning: Remove unwanted columns and handle missing values
train_data <- train_data[c("Age", "Education*", "Annual_Income\n (in Lakhs)", 
                           "Employed", "Own_House", "Own_Vehicle", 
                           "Foreign_Vacation", "Credit_Amount\n(in Lakhs)", 
                           "Defaulter_Status")]

test_data <- test_data[c("Age", "Education*", "Annual_Income\n (in Lakhs)", 
                         "Employed", "Own_House", "Own_Vehicle", 
                         "Foreign_Vacation", "Credit_Amount\n(in Lakhs)", 
                         "Defaulter_Status")]

# Check for and handle any missing values
train_data <- na.omit(train_data)
test_data <- na.omit(test_data)

# Data might require further preprocessing based on specific requirements

# Load necessary library
library(glmnet)

# Prepare the data
train_data <- as.data.frame(train_data)
test_data <- as.data.frame(test_data)

# Convert factors to numeric if necessary
train_data$Defaulter_Status <- as.factor(train_data$Defaulter_Status)

# Fit the logistic regression model
fit <- glm(Defaulter_Status ~ ., data = train_data, family = "binomial")

# Model Evaluation on the test dataset
predictions <- predict(fit, test_data, type = "response")
predictions <- ifelse(predictions > 0.5, 1, 0)
confusionMatrix(as.factor(predictions), as.factor(test_data$Defaulter_Status))

# Additional metrics can be added as per requirement



# Load necessary library
library(caret)

# Fit the linear regression model
fit <- lm(Defaulter_Status ~ ., data = train_data)

# Model Evaluation on the test dataset
predictions <- predict(fit, test_data)

# Since the original problem is a classification problem, you might need to convert these
# predictions to binary form (0 or 1) for meaningful comparison
binary_predictions <- ifelse(predictions > 0.5, 1, 0)

# Compare the binary predictions with the actual values
confusionMatrix(as.factor(binary_predictions), as.factor(test_data$Defaulter_Status))

# Note: Linear regression is not typically used for binary classification problems

# Load necessary library
library(ggplot2)

# Histogram of Ages
ggplot(train_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  ggtitle("Histogram of Ages") +
  xlab("Age") +
  ylab("Frequency")

# Box Plot of Annual Income by Defaulter Status
ggplot(train_data, aes(x = as.factor(Defaulter_Status), y = `Annual_Income\n (in Lakhs)`)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  ggtitle("Box Plot of Annual Income by Defaulter Status") +
  xlab("Defaulter Status (0 = No, 1 = Yes)") +
  ylab("Annual Income (in Lakhs)")

# Scatter Plot of Credit Amount vs. Age
ggplot(train_data, aes(x = Age, y = `Credit_Amount\n(in Lakhs)`)) +
  geom_point(aes(color = as.factor(Defaulter_Status))) +
  ggtitle("Scatter Plot of Credit Amount vs. Age") +
  xlab("Age") +
  ylab("Credit Amount (in Lakhs)") +
  scale_color_manual(values = c("red", "green"), 
                     name = "Defaulter Status",
                     labels = c("No", "Yes"))

# Bar Chart of Education Levels
ggplot(train_data, aes(x = as.factor(`Education*`))) +
  geom_bar(fill = "orange", color = "black") +
  ggtitle("Bar Chart of Education Levels") +
  xlab("Education Level") +
  ylab("Count") +
  scale_x_discrete(labels = c("0" = "Not Educated", "1" = "School Level", 
                              "2" = "Graduate", "3" = "Post Graduate"))

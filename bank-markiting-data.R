# Install the necessary packages if not already installed
required_packages <- c("dplyr", "ggplot2", "tidyverse", "psych", "caret")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies=TRUE)

library(dplyr)
library(ggplot2)
library(tidyverse)
library(psych)
library(caret)

###Data Wrangling
# Determine the path of the dataset
file_path <- "C:/Users/Administrator/Downloads/bank_full.csv"

#لقراءة ملف csv الدالة read.csvماتنفع لان الفواصل ; 
# Standardize variables to suitable type and convert the categorical data into factor by using stringsAsFactors=TRUE 
source_data <- read.csv(file_path, header=TRUE, sep=";", stringsAsFactors=TRUE)
str(source_data)
bank_data<- source_data


#Checking of missing data with printing the result
# Calculate the number of missing data
missing_count <- sum(is.na(bank_data))

#Checking of missing data 
if (missing_count == 0) {
  print("No missing values in data")
} else {
  print("Missing values found in data:")
  print(missing_count)
}

# Apply filtering techniques
#we can use a lot of filtering based on variables which can help us analyze the data
# filtering all customers with a balance higher than 2000
high_balance_customers <- bank_data %>% filter(balance > 2000)
head(high_balance_customers)

# filtering all customers with a balance higher than 1000 and outcome of the previous marketing campaign is a success

balanceabove1000_successpoutcome <- bank_data %>% 
  filter(balance > 1000 & poutcome == "success")

# View the data
head(balanceabove1000_successpoutcome)

# Apply subsetting techniques
#we can use a lot of subsetting based on variables which can help us analyze the data
# for example subsetting base on the factor outcome of the previous marketing campaign (poutcome)

success_poutcome_data <- subset(bank_data, poutcome == "success")
# View the data
head(success_poutcome_data)

# Generate summary statistics:
summary(bank_data)


# Use describe() from psych Package for More Detailed Stats
# Generate descriptive statistics
describe(bank_data)



## Exploratory Data Analysis (EDA) and Visualization
# Create a bar plot for y
ggplot(bank_data, aes(x = y, fill = y)) +
  geom_bar() +
  labs(title = "Subscription Status Distribution (y)", x = "Subscription (y)", y = "Count") +
  theme_minimal()



# Summary Statistics for balance
summary(bank_data$y)



# Visualize the distribution of AGE
ggplot(bank_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "yellow", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Age", x = "Age", y = "Count") +
  theme_minimal()


# Summary Statistics of age
summary(bank_data$age)



#Density Plot of Balance
# View Distribution of balance (Density Plot)
ggplot(bank_data, aes(x = balance)) +
  geom_density(fill = "yellow") +
  labs(title = "Density Plot of Balance for all Customers",
       x = "Balance", y = "Density") +
  theme_minimal()


# Summary Statistics for balance
summary(bank_data$balance)


#Density Plot of Balance for Subscribed Customers
# Filter the data only `y == "yes"`
Subscribed_Customers_data <- bank_data %>% filter(y == "yes")

# View Distribution of balance (Density Plot)
ggplot(Subscribed_Customers_data, aes(x = balance)) +
  geom_density(fill = "yellow") +
  labs(title = "Density Plot of Balance for Subscribed Customers",
       x = "Balance", y = "Density") +
  theme_minimal()



# Summary Statistics of balance for Subscribed Customers
summary(Subscribed_Customers_data$balance)


#Boxplot of Balance VS Subscription (y)
ggplot(bank_data, aes(x = y, y = balance, fill = y)) +
  geom_boxplot() +
  labs(title = "Boxplot of Balance VS Subscription (y)",
       x = "Subscription in term deposit (y)", y = "Balance") +
  theme_minimal()



#Boxplot Balance VS Subscribed Customers (y = 'yes')
# Filter the data only `y == "yes"`
Subscribed_Customers_data <- bank_data %>% filter(y == "yes")

# Create Boxplot for balance (only if y == "yes")
ggplot(Subscribed_Customers_data, aes(x = y, y = balance, fill = y)) +
  geom_boxplot() +
  labs(title = "Boxplot (Balance VS Subscribed Customers) (y = 'yes')", x = "Subscription in term deposit (y)", y = "Balance") +
  theme_minimal() 



# bar plot shows the number of customers in each marital category and their subscription status.
ggplot(bank_data, aes(x = marital, fill = y)) +
  geom_bar(position = "dodge") + 
  # Dodged bars (side by side)
  labs(title = "Subscription Status VS Marital Status",
       x = "Marital Status", y = "Count", fill = "Subscription (y)") +
  theme_minimal()


#Boxplot of marital VS Subscription (y)
ggplot(bank_data, aes(x = y, y = marital, fill = y)) +
  geom_boxplot() +
  labs(title = "Boxplot of Marital Status VS Subscription (y)", 
       x = "Subscription (y)", y = "Marital Status")+
  theme_minimal()

# bar plot shows the number of customers in each job category and their subscription status.
ggplot(bank_data, aes(x = job, fill = y)) +
  geom_bar(position = "dodge") + 
  # Dodged bars (side by side)
  labs(title = "Subscription Status VS job Status",
       x = "Job Status", y = "Count", fill = "Subscription (y)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))



# bar plot the outcome of the previous marketing campaign and their subscription status.
ggplot(bank_data, aes(x = poutcome, fill = y)) +
  geom_bar(position = "dodge") + 
  # Dodged bars (side by side)
  labs(title = "Subscription Status VS outcome of the previous marketing campaign",
       x = "outcome of the previous marketing campaign ", y = "Count", fill = "Subscription (y)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

# Section 1: Detect Anomalies in Balance
# Boxplot to detect outliers in balance
boxplot(bank_data$balance, main="average yearly balance Outliers", horizontal=TRUE)

# Summary statistics of balance
summary(bank_data$balance)

# Identify customers with exceptionally high balances (>80,000)
extreme_balance_customers <- bank_data %>% filter(balance > 80000)

# Section 2: Analyze Patterns in Subscription
# High-balance customers and their subscription likelihood
high_balance_subscribers <- bank_data %>% filter(balance > 40000, y == "yes")
high_balance_subscription_rate <- nrow(high_balance_subscribers) / nrow(bank_data %>% filter(balance > 40000))
print(paste("High-balance subscription rate:", high_balance_subscription_rate))

# Segment customers by balance group and analyze subscription rates
balance_segments <- bank_data %>%
  mutate(balance_category = cut(balance, breaks = c(-Inf, 0, 10000, 40000, Inf),
                                labels = c("Negative", "Low", "Medium", "High"))) %>%
  group_by(balance_category, y) %>%
  summarise(count = n())
print(balance_segments)

# Section 3: Visualize High-Balance Subscribers
ggplot(bank_data %>% filter(balance > 40000), aes(x = balance, fill = y)) +
  geom_histogram(binwidth = 5000, position = "dodge", alpha = 0.7) +
  labs(title = "Subscription Distribution for High-Balance Customers", 
       x = "Balance", y = "Count") +
  theme_minimal()

# Insights Summary:
# 1. Most customers maintain low balances (<10,000).
# 2. Customers with high balances (>40,000) have a high subscription rate (~X%).
# 3. Low-balance customers may need tailored campaigns with flexible options.
# 4. Balance is a strong predictor for subscription and should be included in predictive models.

### Statistical Analysis 
# Step 1: Load Data
# Set file path 
file_path <- "C:/Users/Administrator/Downloads/bank_full.csv"
# Read dataset
bank_data <- read.csv(file_path, sep = ";", stringsAsFactors = TRUE)
# Convert subscription variable `y` to a factor (for classification)
bank_data$y <- as.factor(bank_data$y)

# Step 2: Statistical Tests
# 1. Correlation Analysis: Balance vs Age
#Used to measure the relationship between two numeric variables
cor_test_result <- cor.test(bank_data$balance, bank_data$age)
print(cor_test_result)

# 2. Chi-Square Test: Job vs Subscription
#Used to test the association between two categorical variables.
job_subscription_table <- table(bank_data$job, bank_data$y)
chi_test_result <- chisq.test(job_subscription_table)
print(chi_test_result)

# 3. T-Test: Balance by Subscription
#Used to compare the means of a numeric variable (e.g., balance) across two groups (y = yes/no).
t_test_result <- t.test(balance ~ y, data = bank_data)
print(t_test_result)

# 4. ANOVA: Balance Across Education Levels
#Used to compare balance across more than two groups (e.g., education levels).
anova_model <- aov(balance ~ education, data = bank_data)
print(summary(anova_model))

# Step 3: Logistic Regression (Subscription Prediction)
# Fit logistic regression model
logistic_model <- glm(y ~ balance + age + job + education + marital, 
                      data = bank_data, family = "binomial")

# Display model summary to predict subscription (y) based on numeric and categorical factors.
print(summary(logistic_model))

# Add predicted probabilities to the dataset
bank_data$predicted_prob <- predict(logistic_model, type = "response")

# Visualize predicted probabilities vs balance
ggplot(bank_data, aes(x = balance, y = predicted_prob)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "red") +
  labs(title = "Probability of Subscription by Balance", x = "Balance", y = "Predicted Probability") +
  theme_minimal()

# Evaluate Logistic Model Performance
predicted_classes <- ifelse(bank_data$predicted_prob > 0.5, "yes", "no")
conf_matrix <- confusionMatrix(as.factor(predicted_classes), bank_data$y)
print(conf_matrix)

# Step 4: Linear Regression (Balance Prediction)
# Fit a linear regression model
linear_model <- lm(balance ~ age + job + education + marital, data = bank_data)

# Display model summary to predict balance based on age, job, education, etc.
print(summary(linear_model))

# Visualize residuals to check model assumptions
plot(linear_model, which = 1) # Residuals vs Fitted

# Step 5: Subscription by Balance Groups
# Segment customers by balance group
balance_segments <- bank_data %>%
  mutate(balance_category = cut(balance, breaks = c(-Inf, 0, 10000, 40000, Inf),
                                labels = c("Negative", "Low", "Medium", "High"))) %>%
  group_by(balance_category, y) %>%
  summarise(count = n())
print(balance_segments)


# Visualize subscription rates across balance groups
ggplot(balance_segments, aes(x = balance_category, y = count, fill = y)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Subscription Rates Across Balance Groups", x = "Balance Category", y = "Count") +
  theme_minimal()

# Step 6: Summary of Insights
# Display key insights
cat("\n=== Key Insights ===\n")
cat("1. Correlation test results indicate the relationship between balance and age.\n")
cat("2. Chi-square test results reveal if job type impacts subscription likelihood.\n")
cat("3. Logistic regression identifies key predictors of subscription.\n")
cat("4. Linear regression shows how balance is influenced by age, job, and education.\n")
cat("5. Subscription analysis by balance group highlights target customer segments.\n")


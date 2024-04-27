## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
# Install and import package.
# Load the CSV file.
# View the data frame
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
# View subset of original data frame
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.

###############################################################################

# 1. Load and explore the data
getwd()
setwd(dir='/Users/pmtguest/desktop/LSE/DA301/Assignment Files')

# Install and import Tidyverse.
install.packages("tidyverse")
library(tidyverse)

# Import the data set.
df <- read.csv('turtle_sales.csv', header=TRUE)

# Print the data frame.
View(df)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
subset <- df[, colnames(df)[c(2:3, 7:9)]]

# View the data frame.
View(subset)

# View the descriptive statistics.
summary(subset)

################################################################################

# 2. Review plots to determine insights into the data set.
library(ggplot2)
print(names(subset))

## 2a) Scatterplots
# Create scatterplots.
ggplot(df, aes(x = NA_Sales, y = EU_Sales, colour = Platform)) +
  geom_point()

## 2b) Histograms
# Create histograms.
ggplot(df, aes(x=Product)) + geom_histogram()
ggplot(df, aes(x=Year)) + geom_histogram()

## 2c) Boxplots
# Create boxplots.
ggplot(df, aes(x = Global_Sales, y = Platform)) +
  geom_boxplot()
ggplot(df, aes(x = EU_Sales, y = Platform)) +
  geom_boxplot()
ggplot(df, aes(x = NA_Sales, y = Platform)) +
  geom_boxplot()

###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.


###############################################################################

# 2. Determine the impact on sales per product_id.
library(dplyr)

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales_per_product <- subset %>%
  group_by(Product) %>%
  summarise(total_sales = sum(Global_Sales))

# View the data frame.
View(sales_per_product)

# Explore the data frame.
summary(sales_per_product)


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
scatterplot <- ggplot(sales_per_product, aes(x = Product, y = total_sales)) +
  geom_point()
# Print the scatterplot
print(scatterplot)

# Create histograms.
histogram1 <- ggplot(sales_per_product, aes(x=Product)) + geom_histogram()
histogram2 <- ggplot(sales_per_product, aes(x=total_sales)) + geom_histogram()
# Print the histograms
print(histogram1)
print(histogram2)

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(sales_per_product$total_sales,
       col='blue',
       xlab="z Value",
       ylab='Sales')
qqline(sales_per_product$total_sales,
       col='red',
       lwd=2)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages("moments")
library (moments)

# Perform Shapiro-Wilk test.
shapiro.test(sales_per_product$total_sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales_per_product$total_sales) 
kurtosis(sales_per_product$total_sales)


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

cor(sales_per_product)
plot(sales_per_product$Product, sales_per_product$total_sales)

model1 <- lm(total_sales ~ Product, data = sales_per_product)

summary(model1)

plot(model1$residuals)

abline(coefficients(model1))

sales_per_product <- mutate(sales_per_product,
                            logtotal_sales = log(total_sales))
model2 <- lm(logtotal_sales ~ Product, data = sales_per_product)

summary(model2)

plot(sales_per_product$Product, sales_per_product$logtotal_sales)

abline(coefficients(model2))

## 2b) Create a plot (simple linear regression)
# Basic visualisation.


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
summary(df)
newsubset <- df[, colnames(df)[c(1:2, 4, 7:9)]]

View(newsubset)
cor(newsubset)

# Install the psych package.
install.packages('psych')

# Import the psych package.
library(psych)

# Use the corPlot() function.
# Specify the data frame (newsubset) and set 
# character size (cex=2).
corPlot(newsubset, cex=1)

# Create a new object and 
# specify the lm function and the variables.
newModel = lm(Global_Sales ~ NA_Sales + EU_Sales, data=newsubset)
# Print the summary statistics.
summary(newModel)

# Add new variables.
newModel2 = lm(Global_Sales ~ NA_Sales + EU_Sales + Ranking
               + Product + Year, data=newsubset)

# Change the model name.
summary(newModel2)

# Adapt the model
newModel3 = lm(Global_Sales ~ NA_Sales + EU_Sales + Ranking
               + Product, data=newsubset)

# Change the model name.
summary(newModel3)

# Adapt the model
newModel4 = lm(Global_Sales ~ NA_Sales + EU_Sales + Year
               + Product, data=newsubset)

# Change the model name.
summary(newModel4)

# newModel2 has the highest Adjusted R-squared, so newModel2 is the strongest
# Multiple linear regression model.

# Create a new object and specify the predict function.
predictTest = predict(newModel2, newdata=newsubset,
                      interval='confidence')

# Print the object.
predictTest

# Create a new data frame with new values for predictor variables
new_data <- data.frame(
  NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
  EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))

predictions <- predict(newModel, newdata = new_data)

print(predictions)

###############################################################################

##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80
## predicted Global_Sales = 71.47
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
## predicted Global_Sales = 6.86
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
## predicted Global_Sales = 4.25
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
## predicted Global_Sales = 4.13
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
## predicted Global_Sales = 26.43


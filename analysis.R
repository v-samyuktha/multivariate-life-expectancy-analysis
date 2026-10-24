setwd("set-working-directory-here")
data = read.csv("set-file-name-here.csv")

library(ggcorrplot)
library(psych)
library(dplyr)
library(ggpubr)
library(regclass)
library(lmtest)
library(ridge)


#EDA
colnames(data)
summary(data)

head(data)
tail(data)

#Impute using Mean
sum(is.na(data))

numeric_cols <- sapply(data, is.numeric)
numeric_data <- data[, numeric_cols]
non_numeric_data <- data[, !numeric_cols]

sum(is.na(numeric_data))

for (col in names(numeric_data)) {
  col_mean <- mean(numeric_data[[col]], na.rm = TRUE)
  numeric_data[[col]][is.na(numeric_data[[col]])] <- col_mean
}
data[, numeric_cols] <- numeric_data

sum(is.na(data))

#Select data between 2010-2015
table(data$Year)
library(tidyverse)
df = data %>% filter(
  Year=="2010" | Year =="2011" | Year=="2012" | Year == "2013" | Year == "2014" | Year =="2015"
)

table(df$Year)
dim(df)

library(lmtest)
library(RColorBrewer)
library(regclass)
library(ridge)
library(psych)
library(factoextra)
library(olsrr)

#Ordinal encoding of Status
df$Status <- factor(df$Status, levels = c("Developing", "Developed"))
df$Status_encoded <- as.numeric(df$Status)
table(df$Status_encoded)

#Plotting histogram of dependent variable
n_colors <- 9
palette <- brewer.pal(n_colors, "YlGnBu")
hist(df$Life.expectancy, breaks = 10, xlab = 'Life Expectancy', 
     main = "Histogram of Life Expectancy", col = palette)

#Checking normality of the dependent variable
qqnorm(df$Life.expectancy)
qqline(df$Life.expectancy, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7,col=2,lwd=2)
shapiro.test(df$Life.expectancy)


# Square root transformation
transformed_data <- (df$Life.expectancy)^2

# Shapiro-Wilk test on transformed data
shapiro.test(transformed_data)

df <- df %>%
  mutate(Life.expectancy = Life.expectancy ^ 2)

# Simple Linear Regression
fit1 = lm(Life.expectancy ~ GDP, data= df)
summary(fit1)
plot(df$GDP, df$Life.expectancy, main = "Life Expectancy vs GDP",
     xlab = "GDP", ylab = "Life Expectancy")
abline(fit1, col = 'red', lwd = 3)

#Multiple Linear Regression
fit_data = lm( Life.expectancy ~  Adult.Mortality + Alcohol + Hepatitis.B + BMI + Polio + 
                 Diphtheria + GDP + thinness..1.19.years + Income.composition.of.resources + 
                 infant.deaths + percentage.expenditure + Measles + under.five.deaths + 
                 Total.expenditure + HIV.AIDS + Population + thinness.5.9.years + Schooling + Status_encoded,
               data= df)
summary(fit_data)
fit_data = step(fit_data,direction="backward",trace=1)
summary(fit_data)
plot(fitted(fit_data), residuals(fit_data),main = "Residuals vs Fitted (Updated Multiple Linear Regression Model)",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2, col = 'red')
mse <- summary(fit_data)$sigma^2
mse

#Homoscedasticity
gqtest(fit_data, data = df, alternative = "two.sided")
plot(fitted(fit_data), rstandard(fit_data), ylab = "Residuals", xlab = "Fitted Values")
abline(h = 0, lty = 5, col = 'red')  # Add a horizontal line at y=0

#Multicollinearity
df_col<- df[,c("GDP", "Status_encoded", "infant.deaths", "percentage.expenditure", "Adult.Mortality")]
df_col
round(cor(df_col), 2)
VIF(fit_data)

#Corrplot

df <- subset(df, select = -Country)
df <- subset(df, select = -Year)
df <- subset(df, select = -Status)
colnames(df)

# Calculate correlation matrix
correlation_matrix <- cor(df)

# Convert correlation matrix to long format
library(reshape2)
correlation_matrix_long <- melt(correlation_matrix)

# Plot correlation using ggplot2
library(ggplot2)
ggplot(correlation_matrix_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed()

#Ridge Regression
fit2 = linearRidge(Life.expectancy ~ Adult.Mortality + Alcohol + Hepatitis.B + BMI + Diphtheria +GDP + thinness..1.19.years + Income.composition.of.resources + infant.deaths + Measles + under.five.deaths + Total.expenditure + HIV.AIDS + Schooling + Status_encoded, data= df)
summary(fit2)
fitted_values <- predict(fit2)
residuals <- df$Life.expectancy - fitted_values
plot(fitted_values, residuals, main = "Residuals vs Fitted (Linear Ridge Regression)",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2, col = 'red')
mse <- mean((residuals)^2)
mse

#dropping infant.deaths
fit_data = lm( Life.expectancy ~  Adult.Mortality + Alcohol + Hepatitis.B + BMI + Polio + 
                 Diphtheria + GDP + thinness..1.19.years + Income.composition.of.resources + 
                 percentage.expenditure + Measles + under.five.deaths + 
                 Total.expenditure + HIV.AIDS + Population + thinness.5.9.years + Schooling + Status_encoded,
               data= df)
summary(fit_data)
fit_data = step(fit_data,direction="backward",trace=1)
summary(fit_data)
VIF(fit_data)

library(MASS)
#boxcox
boxcox_result <- boxcox(Life.expectancy ~ Adult.Mortality + Alcohol + Hepatitis.B + BMI + Polio + 
                          Diphtheria + Income.composition.of.resources + 
                         Measles + under.five.deaths + 
                          Total.expenditure + HIV.AIDS + Schooling + Status_encoded, 
                        data = df)

#best lambda = around 0.5
df <- df %>%
  mutate(Life.expectancy = Life.expectancy ^ 2)

fit2 = linearRidge(Life.expectancy ~ Adult.Mortality + Alcohol + Hepatitis.B + BMI + Diphtheria +GDP + thinness..1.19.years + Income.composition.of.resources + infant.deaths + Measles + under.five.deaths + Total.expenditure + HIV.AIDS + Schooling + Status_encoded, data= df)
summary(fit2)
fitted_values <- predict(fit2)
residuals <- df$Life.expectancy - fitted_values
plot(fitted_values, residuals, main = "Residuals vs Fitted (Linear Ridge Regression)",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2, col = 'red')
mse <- mean((residuals)^2)
mse

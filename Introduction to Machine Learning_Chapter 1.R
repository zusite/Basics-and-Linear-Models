#Basics and Linear Models
#https://htmlpreview.github.io/?https://github.com/mayer79/ml_lecture/blob/master/r/1_Basics_and_Linear_Models.html
#Notes


# *************************************************************************************************************
# 1. Basics

# color with ordered categories D < E < F < G < H < I < J,
# cut with ordered categories Fair < Good < Very Good < Premium < Ideal, and
# clarity with ordered categories I1 < SI2 < SI1 < VS2 < VS1 < VVS2 < VVS1 < IF.

library(ggplot2)
head(diamonds)
?diamonds

levels(diamonds$color)

library(dplyr)

# Calculate the average price by color
avgpricebycolor <-diamonds %>% 
  group_by(clarity) %>% 
  summarise(mean=mean(price), min=min(price), max=max(price),q3=quantile(price,0.75),count = n()) %>% 
  mutate(sqrt_count = count^0.5,count_rel=sqrt_count/mean(sqrt_count))

?summarise

plot(avgpricebycolor$color,avgpricebycolor$mean)

# Univariate description
summary(diamonds[, colnames(diamonds)])

# Bar charts provide a visual presentation of categorical data
# Histograms are used to plot density of interval (usually numeric) data
ggplot(diamonds, aes(x=price)) + geom_histogram(fill = "green")
ggplot(diamonds, aes(x=color)) + geom_bar(fill = "green")

# Selected bivariate descriptions
# The point geom is used to create scatterplots
# The scatterplot is most useful for displaying the relationship between two continuous variables

ggplot(diamonds, aes(x=carat, y=price)) + 
  geom_point(alpha=1, shape=".",color="black") + 
  ggtitle("Price against weight")

ggplot(diamonds, aes(y = log(price), x = log(carat))) +
  geom_point(alpha = 0.5, shape = ".", color = "black") +
  ggtitle("Price against carat (log-log-scale)")

ggplot(diamonds, aes(x=color,y=price)) +
  geom_boxplot(fill="green")+
  ggtitle("Price against color")

# If FALSE (default) make a standard box plot.
# If TRUE, boxes are drawn with widths proportional to the square-roots of the number of observations in the groups

ggplot(diamonds, aes(y = price, x = clarity)) +
  geom_boxplot(fill = "orange", varwidth = TRUE) +
  ggtitle("Price against clarity")

# Prices tend to be lower for nice colors, nice cuts as well as for nice clarities.
# This unintuitive behaviour will be entangled later by our regression models.


# *************************************************************************************************************
# 2. Linear Regression

fit <- lm(price~carat, data=diamonds)
summary(fit)

intercept <- coef(fit)[[1]]
slope <- coef(fit)[[2]]

# Visualize the regression line
ggplot(diamonds, aes(y = price, x=carat)) + 
  geom_point(alpha = 0.5, shape = ".") +
  coord_cartesian(xlim = c(0, 3), ylim = c(-3000, 20000)) +
  geom_abline(slope = slope, intercept = intercept, color = "blue", size = 1) +
  ggtitle("Simple Linear Regression")

predict(fit, data.frame(carat=1.3))

mse <- function(y, pred) {
  mean((y - pred)^2)
}

(MSE <- mse(diamonds$price, predict(fit, diamonds)))


empty_model <- lm(price ~ 1, data = diamonds)  # predictions equal mean(diamonds$price)
MSE_empty <- mse(diamonds$price, predict(empty_model, diamonds))

# R-squared
(MSE_empty - MSE) / MSE_empty


# If argument ordered is TRUE, the factor levels are assumed to be ordered.
fit <- lm(price~carat+factor(color, order=FALSE), data=diamonds)
summary(fit)


# Cubic regression
library(tidyverse)

fit <- lm(price~poly(carat,3), data=diamonds)
summary(fit)

plot(x=diamonds$carat,y=predict(fit,diamonds))

# Plot effect of carat on average price
data.frame(carat = seq(0.3, 4.5, by = 0.1)) %>% 
  mutate(price = predict(fit, .)) %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point(data = diamonds, shape = ".", alpha = 0.2, color = "green") + 
  geom_line() +
  geom_point()

# I(x^2) would return a vector of values raised to the second power.

fit2 <- lm(price~I(carat)+I(carat^2)+I(carat^3), data=diamonds)
summary(fit2)
plot(x=diamonds$carat,y=predict(fit2,diamonds))

p1 <- c(predict(fit, diamonds))
p2 <- c(predict(fit2, diamonds))
p12 <- data.frame(p1,p2)

p12 <- p12 %>% mutate(difference = p1-p2)
sum(p12$difference)

# The goal is to predict
# the number of shares in social networks (popularity)

# https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity?fbclid=IwAR2DKTSo17Hpo8yMhT1-z80NfKnVJgT5nSObsKDtYoZJEwHJ277bQPa5g5M

library(summarytools)
library(ggplot2)
library(ggcorrplot)
library(corrr)
library(data.table)
library(lmtest)
library(MASS)
library(vip)
library(car)
library(tidyr)
library(dplyr)
library(corrplot)
library(RColorBrewer)
library(tidyverse)
library(caret)
library(reshape2)
library(nortest)

### Data loading
newsPopularityData = read.csv("OnlineNewsPopularity.csv")

# Check for NaN values
sum(is.na(newsPopularityData))
# There are no NaN values in the dataset

#### First, we follow the same analyzing step from activity 1
### Descriptive statistic:
str(newsPopularityData)
# Remove non-predictive attributes
# (url and timedelta)
newsPopularity = newsPopularityData[,-1:-2]

# Correlation heatmap
CR <- cor(newsPopularity)
View(CR)
png("corr_plot.png", width = 6000, height = 6000)
corrplot(CR, addCoef.col = 1, cl.cex = 6,
         tl.cex = 3, number.cex = 0.01)
dev.off()

# n_tokens_content = number of words in the content
# For articles with n_tokens_content = 0, it means that
# they don't have any content -> we drop them since
# their attributes have no meaning in our analysis

# Count number of articles with n_tokens_content == 0
sum(newsPopularity$n_tokens_content == 0)
# Drop these articles
newsPopularity <- newsPopularity[!(newsPopularity$n_tokens_content==0),]

# Drop highly correlated attributes, which can be seen 
# from correlation heatmap
newsPopularity <- subset(newsPopularity,
                         select = -c(n_non_stop_unique_tokens,
                                     n_non_stop_words,
                                     kw_avg_min))

# Understanding each feature variables distribution
stat_newsPop <- descr(as.data.frame(newsPopularity), transpose = TRUE,
                      stats = c('mean', 'sd', 'min', 'max', 'med',
                                'Q1', 'IQR', 'Q3'))
View(stat_newsPop)

# Understanding target variable
targetMed <- median(newsPopularity$shares)
# We use the median as a threshold to decide if an article
# is considered 'popular' or not
newsPopPopular <- newsPopularity[(newsPopularity$shares>=targetMed),]
newsPopUnpopular <- newsPopularity[(newsPopularity$shares<targetMed),]

# We find the relationship with the day of the week
wDay <- c('weekday_is_monday','weekday_is_tuesday',
          'weekday_is_wednesday','weekday_is_thursday',
          'weekday_is_friday','weekday_is_saturday',
          'weekday_is_sunday')
cntPop <- colSums(subset(newsPopPopular, select = wDay))
cntUnpop <- colSums(subset(newsPopUnpopular, select = wDay))
tmpDf <- data.frame(cntPop, cntUnpop)
rownames(tmpDf) <- c("Monday","Tuesday","Wednesday",
                     "Thursday","Friday","Saturday","Sunday")
png("pop_day_median_plot.png", width = 1000, height = 700)
barplot(as.matrix(t(tmpDf)), ylab = 'Count', beside = TRUE,
        col = 1:nrow(t(tmpDf)),
        main = 'Count of popular/unpopular news over different day of week (Median)')
legend("topright", legend = rownames(t(tmpDf)), pch = 15,
       col = 1:nrow(t(tmpDf)))
dev.off()

# We find the relationship with data channel
dChannel <- c('data_channel_is_lifestyle',
              'data_channel_is_entertainment',
              'data_channel_is_bus',
              'data_channel_is_socmed',
              'data_channel_is_tech',
              'data_channel_is_world')

cntPop <- colSums(subset(newsPopPopular, select = dChannel))
cntUnpop <- colSums(subset(newsPopUnpopular, select = dChannel))
tmpDf <- data.frame(cntPop, cntUnpop)
rownames(tmpDf) <- c("Lifestyle","Entertainment","Business",
                     "SocialMedia","Tech","World")
png("pop_channel_median_plot.png", width = 1000, height = 700)
barplot(as.matrix(t(tmpDf)), ylab = 'Count', beside = TRUE,
        col = 1:nrow(t(tmpDf)),
        main = 'Count of popular/unpopular news over different channel (Median)')
legend("topleft", legend = rownames(t(tmpDf)), pch = 15,
       col = 1:nrow(t(tmpDf)))
dev.off()

# Convert from binary categorical variables into 
# ordinal and nominal categorical variables

# weekday - ordinal
newsPopularity$weekday <- copy(newsPopularity$is_weekend)
for(i in 1:nrow(newsPopularity)){
  if (newsPopularity$weekday_is_monday[i] == 1){
    newsPopularity$weekday[i] = 0
  }
  if (newsPopularity$weekday_is_tuesday[i] == 1){
    newsPopularity$weekday[i] = 1
  }
  if (newsPopularity$weekday_is_wednesday[i] == 1){
    newsPopularity$weekday[i] = 2
  }
  if (newsPopularity$weekday_is_thursday[i] == 1){
    newsPopularity$weekday[i] = 3
  }
  if (newsPopularity$weekday_is_friday[i] == 1){
    newsPopularity$weekday[i] = 4
  }
  if (newsPopularity$weekday_is_saturday[i] == 1){
    newsPopularity$weekday[i] = 5
  }
  if (newsPopularity$weekday_is_sunday[i] == 1){
    newsPopularity$weekday[i] = 6
  }
}

#channel - nominal
newsPopularity$channel <- copy(newsPopularity$is_weekend)
for(i in 1:nrow(newsPopularity)){
  if (newsPopularity$data_channel_is_lifestyle[i] == 1){
    newsPopularity$channel[i] = 0
  }
  if (newsPopularity$data_channel_is_entertainment[i] == 1){
    newsPopularity$channel[i] = 1
  }
  if (newsPopularity$data_channel_is_socmed[i] == 1){
    newsPopularity$channel[i] = 2
  }
  if (newsPopularity$data_channel_is_tech[i] == 1){
    newsPopularity$channel[i] = 3
  }
  if (newsPopularity$data_channel_is_world[i] == 1){
    newsPopularity$channel[i] = 4
  }
}
newsPopularity <- newsPopularity %>% select(-dChannel)
newsPopularity <- newsPopularity %>% select(-wDay)

## QQ Plot for weekday and channel
# Weekday
check_normal_weekday <- function(x) {
  temp <- subset(newsPopularity, newsPopularity$weekday == x)
  qqnorm(temp$shares, main = paste("Normal Q-Q Plot of ", x))
  qqline(temp$shares, col = "red")
  ad.test(temp$shares)
}
check_normal_weekday(0)

# The plotting is highly affected by outliers so we remove them
Q1 <- quantile(newsPopularity$shares, .25)
Q3 <- quantile(newsPopularity$shares, .75)
IQR <- IQR(newsPopularity$shares)

no_outliers <- subset(newsPopularity, 
                      newsPopularity$shares > (Q1 - 1.5*IQR) &
                        newsPopularity$shares < (Q3 + 1.5*IQR))

check_normal_weekday_noout <- function(x, name) {
  temp <- subset(no_outliers, no_outliers$weekday == x)
  qqnorm(temp$shares, main = paste("Normal Q-Q Plot of ", name))
  qqline(temp$shares, col = "red")
  ad.test(temp$shares)
}
check_normal_weekday_noout(0,"Monday")
check_normal_weekday_noout(1,"Tuesday")
check_normal_weekday_noout(2,"Wednesday")
check_normal_weekday_noout(3,"Thursday")
check_normal_weekday_noout(4,"Friday")
check_normal_weekday_noout(5,"Saturday")
check_normal_weekday_noout(6,"Sunday")

# Channel
check_normal_channel_noout <- function(x, name) {
  temp <- subset(no_outliers, no_outliers$channel == x)
  qqnorm(temp$shares, main = paste("Normal Q-Q Plot of ", name))
  qqline(temp$shares, col = "red")
  ad.test(temp$shares)
}
check_normal_channel_noout(0,"Lifestyle")
check_normal_channel_noout(1,"Entertainment")
check_normal_channel_noout(2,"Social Media")
check_normal_channel_noout(3,"Tech")
check_normal_channel_noout(4,"World")

# Anderson-Darling test gives very small value for both features which means
# that both features does not follow a normal distribution

## We use one-way ANOVA to check what is the 
# best day to post articles?
newsPopularity_copy <- copy(newsPopularity)
newsPopularity_copy$weekday <- as.factor(newsPopularity_copy$weekday)

one_way_weekday <- aov(shares ~ weekday, data = newsPopularity_copy)
# Levene's test to check the homogeneity of variance assumption

leveneTest(shares ~ weekday, data = newsPopularity_copy)
# Levene's test give p-value = 0.1859, not less than the
# significant level of 0.05 -> no evidence to reject 
# the null hypothesis - variance among groups is equal


# For the hypothesis of normality, with large dataset, 
# we can almost guarantee that we can reject such hypothesis.
# But for the sake of demonstration, we will use
# Shapiro-Wilk test to check normality assumption.
# Since the maximum sample size is 5000, we randomly take
# 5000 samples from the residuals
residual <- sample(residuals(one_way_weekday), 5000)
shapiro.test(residual)
# p-value obtains from shapiro-wilk test is ~= 0,
# we can reject the H_0 and conclude that the data is non-normal
# at very high certainty

summary(one_way_weekday)

# boxplot contain outliers thus cannot portrait the box clearly
# We have to remove outliers
png("oneway_weekday_noout_boxplot.png", width = 1000, height = 700)
boxplot(no_outliers$shares ~ no_outliers$weekday,
        main = "Shares by day", xlab = "Day", ylab = "Shares",
        col = 2:8, las = 1)
dev.off()
# From the boxplot, we can see that saturday has most shares
# (the mean of shares is larger than the other days)

# We cannot confirm this, since useing Tukey's HSD test 
# assumes normal data with equal variances.
# But for the sake of demonstration, we use
# TukeyHSD test to check difference between pairs of groups
TukeyHSD(one_way_weekday)

## Similarly, we do the same procedure for data channel
## We use one-way ANOVA to check what is the 
# best channel to post articles?
# We first create a column 'channel' 
# - an ordinal categorical variable


newsPopularity_copy <- copy(newsPopularity)
newsPopularity_copy$channel <- as.factor(newsPopularity_copy$channel)

one_way_channel <- aov(shares ~ channel, data = newsPopularity_copy)
# Levene's test to check the homogeneity of variance assumption

leveneTest(shares ~ channel, data = newsPopularity_copy)
# Levene's test give p-value ~= 0,  less than the
# significant level of 0.05 -> can reject H_0
# the null hypothesis - variance among groups is equal
# -> variance among groups is not equal

# Shapiro-Wilk test to check normality assumption
# Since the maximum sample size is 5000, we randomly take
# 5000 samples from the residuals
residual <- sample(residuals(one_way_channel), 5000)
shapiro.test(residual)
# p-value obtains from shapiro-wilk test ~= 0,
# we can reject the H_0 and conclude that the data is 
# non-normal at very high certainty
summary(one_way_channel)

no_outliers <- subset(newsPopularity_copy, 
                      newsPopularity_copy$shares > (Q1 - 1.5*IQR) &
                        newsPopularity_copy$shares < (Q3 + 1.5*IQR))
png("oneway_channel_noout_boxplot.png", width = 1000, height = 700)
boxplot(no_outliers$shares ~ no_outliers$channel,
        main = "Shares by channel", xlab = "Channel", ylab = "Shares",
        col = 2:6, las = 1)
dev.off()
# From the boxplot, we can see that Business has most shares
# and World has least shares
# Falling into similar situation with analysis of weekday,
# we don't have the assumption of normal data with equal variances
# TukeyHSD test to check difference between pairs of groups

## We continue with 2-way ANOVA to find the affection of
# weekday and channel on shares
newsPopularity_copy <- copy(newsPopularity)
newsPopularity_copy$channel <- as.factor(newsPopularity_copy$channel)
newsPopularity_copy$weekday <- as.factor(newsPopularity_copy$weekday)

two_way <- aov(shares~channel*weekday,data=newsPopularity_copy)

# LeveneTest to check homogeneity of variance assumption
leveneTest(shares~channel*weekday, data = newsPopularity_copy)
# Levene's test with p-value ~= 0 suggests that the variance
# across groups is statistically different

# Shapiro-Wilk test to check normality assumption
# Since the maximum sample size is 5000, we randomly take
# 5000 samples from the residuals
residual2 <- sample(residuals(object = two_way), 5000)
shapiro.test(x = residual2)
# Shapiro-Wilk test with p-value ~=0 suggests that the
# residuals does not follow normal distribution
summary(two_way)

### Linear regression
# Train-test split
set.seed(1)

random_sample <- createDataPartition(newsPopularity$shares, p=0.8, list=FALSE)
train_data <- newsPopularity[random_sample,]
test_data <- newsPopularity[-random_sample,]

# Simple linear regression
linreg <- lm(shares ~., data = train_data)
predictions <- predict(linreg, test_data)
error_df <- data.frame( R2 = R2(predictions, test_data$shares),
                        RMSE = RMSE(predictions, test_data$shares),
                        MAE = MAE(predictions, test_data$shares))
coef_list <- data.frame(linreg$coefficients)
summary(linreg)
View(error_df)

# Simple linear regression without outliers
Q1 <- quantile(train_data$shares, .25)
Q3 <- quantile(train_data$shares, .75)
IQR <- IQR(train_data$shares)
train_noout <- subset(train_data, 
                      train_data$shares > (Q1 - 1.5*IQR) &
                        train_data$shares < (Q3 + 1.5*IQR))

linreg_noout <- lm(shares ~., data = train_noout)
predictions <- predict(linreg_noout, test_data)
error_noout_df <- data.frame( R2 = R2(predictions, test_data$shares),
                              RMSE = RMSE(predictions, test_data$shares),
                              MAE = MAE(predictions, test_data$shares))
coef_list_noout <- data.frame(linreg_noout$coefficients)
summary(linreg_noout)
View(error_noout_df)

# R-squared does improve on train set but not on test set
# By remove outliers does not improve R2 and MSE on test
# So we use our final coefficients of simple linear regression

## Try Ridge and Lasso regression
library(glmnet)
library(MLmetrics)

# Ridge
set.seed(1)
train.matrix <- model.matrix(shares ~., data = train_data)
test.matrix <- model.matrix(shares ~., data = test_data)
# tune the lambda parameter
grid <- 10^seq(3, -2, by = -0.1)
ridge.mod <- cv.glmnet(train.matrix, train_data[, "shares"], alpha = 0, lambda = grid, standardize = TRUE)
# choose the best lambda
bestlam.ridge <- ridge.mod$lambda.min
print(bestlam.ridge)
# Calculate train and test RMSE + R^2 score
y_train_pred <- predict(ridge.mod, s = bestlam.ridge, newx = train.matrix)
y_test_pred <- predict(ridge.mod, s = bestlam.ridge, newx = test.matrix)
print(RMSE(test_data[, "shares"], y_test_pred)) # Test RMSE
print(R2_Score(y_test_pred, test_data[, "shares"])) # Test R^2 score
# examine the coefficient estimates
predict(ridge.mod, type = "coefficients", s = bestlam.ridge) 

# Lasso
set.seed(1)
train.matrix <- model.matrix(shares ~., data = train_data)
test.matrix <- model.matrix(shares ~., data = test_data)
# tune the lambda parameter
grid <- 10^seq(3, -2, by = -0.1)
lasso.mod <- cv.glmnet(train.matrix, train_data[, "shares"], alpha = 1, lambda = grid, standardize = TRUE)
# choose the best lambda
bestlam.lasso <- lasso.mod$lambda.min
print(bestlam.lasso)
# Calculate train and test RMSE + R^2 score
y_train_pred <- predict(lasso.mod, s = bestlam.lasso, newx = train.matrix)
y_test_pred <- predict(lasso.mod, s = bestlam.lasso, newx = test.matrix)
print(RMSE(test_data[, "shares"], y_test_pred)) # Test RMSE
print(R2_Score(y_test_pred, test_data[, "shares"])) # Test R^2 score
# examine the coefficient estimates
predict(lasso.mod, type = "coefficients", s = bestlam.lasso) 

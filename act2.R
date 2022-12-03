# The goal is to predict
# the number of shares in social networks (popularity)

# https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity?fbclid=IwAR2DKTSo17Hpo8yMhT1-z80NfKnVJgT5nSObsKDtYoZJEwHJ277bQPa5g5M



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
library(summarytools)
library(tidyverse)
library(caret)
library(ggplot2)
library(reshape2)

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

## We use one-way ANOVA to check what is the 
# best day to post articles?
# We first create a column 'weekday' 
# - an ordinal categorical variable

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
newsPopularity <- newsPopularity %>% select(-wDay)
newsPopularity_copy <- copy(newsPopularity)
newsPopularity_copy$weekday <- as.factor(newsPopularity_copy$weekday)

one_way_weekday <- aov(shares ~ weekday, data = newsPopularity_copy)
# Levene's test to check the homogeneity of variance assumption

leveneTest(shares ~ weekday, data = newsPopularity_copy)
# Levene's test give p-value = 0.1859, not less than the
# significant level of 0.05 -> no evidence to reject 
# the null hypothesis - variance among groups is equal

# Shapiro-Wilk test to check normality assumption
# Since the maximum sample size is 5000, we randomly take
# 5000 samples from the residuals
residual <- sample(residuals(one_way_weekday), 5000)
shapiro.test(residual)
# p-value obtains from shapiro-wilk test is significantly < 0.01,
# we can reject the H_0 and conclude that the data is non-normal
# at very high certainty

summary(one_way_weekday)
png("oneway_weekday_boxplot.png", width = 1000, height = 700)
boxplot(newsPopularity_copy$shares ~ newsPopularity_copy$weekday,
        main = "Shares by day", xlab = "Day", ylab = "Shares",
        col = 1:7, las = 1)
dev.off()
# boxplot contain outliers thus cannot portrait the box clearly
# We have to remove outliers
Q1 <- quantile(newsPopularity_copy$shares, .25)
Q3 <- quantile(newsPopularity_copy$shares, .75)
IQR <- IQR(newsPopularity_copy$shares)

no_outliers <- subset(newsPopularity_copy, 
                      newsPopularity_copy$shares > (Q1 - 1.5*IQR) &
                        newsPopularity_copy$shares < (Q3 + 1.5*IQR))
png("oneway_weekday_noout_boxplot.png", width = 1000, height = 700)
boxplot(no_outliers$shares ~ no_outliers$weekday,
        main = "Shares by day", xlab = "Day", ylab = "Shares",
        col = 2:8, las = 1)
dev.off()
# From the boxplot, we can see that saturday has most shares
# (the mean of shares is larger than the other days)
# To confirm this, we use Tukey's HSD test to test the
# differences between pairs of days 
# TukeyHSD test to check difference between pairs of groups
TukeyHSD(one_way_weekday)
# Although the pairs 5-1 and 5-3 < 0.05 and pair 5-4 and 5-2 < 0.1
# Pair 5-0 = 0.6327 shows that the effect of Monday and Saturday
# are not statistically different from each other.
# We then cannot confirm the initial conjecture that
# Saturday is the best day


## Similarly, we do the same procedure for data channel
## We use one-way ANOVA to check what is the 
# best channel to post articles?
# We first create a column 'channel' 
# - an ordinal categorical variable

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
# To confirm this, we use Tukey's HSD test to test the
# differences between pairs of days 
# TukeyHSD test to check difference between pairs of groups
TukeyHSD(one_way_channel)
# We cannot support that Business has most shares but
# We can support World has least shares with ~=0 significance level


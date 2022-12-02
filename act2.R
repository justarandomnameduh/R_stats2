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

# We use one-way ANOVA to check relationship
# between shares and days of the week
# We first create a column 'weekday' - an ordinal categorical variable

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

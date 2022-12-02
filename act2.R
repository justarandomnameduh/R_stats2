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

### Data loading
newsPopularityData = read.csv("OnlineNewsPopularity.csv")

# Check for NaN values
sum(is.na(newsPopularity))
# There are no NaN values in the dataset

#### First, we follow the same analyzing step from activity 1
### Descriptive statistic:
str(newsPopularity)
# Remove non-predictive attributes
newsPopularity = newsPopularityData[,-1:-2]
to_remove <- c("char1", "char2")
my_df[ , !(names(my_df) %in% to_remove)]



stat_newsPop <- descr(as.data.frame(newsPopularity), transpose = TRUE,
      stats = c('mean', 'sd', 'min', 'max', 'med',
                'Q1', 'IQR', 'Q3'))

                       

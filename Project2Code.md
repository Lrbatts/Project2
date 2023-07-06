Project2
================
Landon Batts, Jose Singer-Freeman
2023-07-02

## 1.Introducton

In this project we work with the [online news popularity data
set](https://archive.ics.uci.edu/dataset/332/online+news+popularity)
published by UC Irvine The data set summarizes a heterogeneous set of
features about articles published by Mashable in a period of two years.

Our goal is to predict the number of “shares” (popularity) of articles
in social networks. This is done separately for 6 different types or
“channels” of articles, namely, lifestyle, entertainment, business,
social media, tech and world news.

Our predictive models for shares consist of two linear regression
models, a random forest model and a boosted tree model for each of the 6
channels.

## 2. Import Data

``` r
#Get data
raw_data<-read.csv("~/ST558/Project2/OnlineNewsPopularity.csv")

# Remove variables we won't use
newsData<-raw_data %>% select(-c(url,timedelta))

#Filter data from a single channel (just as test) using the parameter params$channel

#store business channel the global channel parameter 
params$channel<-"data_channel_is_bus"

# convert the parameter into a name and then select the rows  where the channel is 1
channelData<-newsData %>% filter(eval(as.name(params$channel))==1)
#shares imported as integer.  Convert to double for purposes of analysis.. 
chanellData<-channelData%>%mutate(shares=as.numeric(shares))
```

## 3. Basic Summary Statistics

The table below shows pairs of variables whose correlation, in absolute
terms, is above 0.5. For those that exceed 0.9, we will drop a member of
each pair of variables from our models, namely, kw_avg_min,
n_non_stop_unique_tokens, and rate_negative_words.

### Summary Statistics

``` r
#Correlation

tempordata<-channelData[-c(12:17)] #remove channel variables

corMat<-cor(tempordata, use="pairwise.complete.obs")

#get correlation of shares with other variables
  
share_cor <- data.frame("r"=corMat["shares",])
share_cor<-share_cor%>%
  rownames_to_column("variable")%>%
  filter(!variable=="shares")%>%
  arrange(desc(r))

# Find highest correlations (above 0.5).  First drop higher triangle to avoid duplicates and remove the diagonal. 
corMat[lower.tri(corMat,diag=TRUE)] <- NA  # drop upper triangle
corMat[corMat == 1] <- NA  #drop perfect correlations

corMat <- as.data.frame(as.table(corMat)) #form a  table with 3 columns: 2-variables and their correlation
corMat <- na.omit(corMat) #remove missing values

corMat<-subset(corMat, abs(Freq) > 0.5) #select correlation values above 0.5  
corMat <- corMat[order(-abs(corMat$Freq)),] #sort by highest to lowest correlation


#turn corr back into matrix in order to plot with corrplot
  corMat2 <- reshape2::acast(corMat, Var1~Var2, value.var="Freq") #melt the data

  #Print table and chart for correlations among predictors

  knitr::kable(corMat, col.names=as.vector(c("Variable 1", "Variable 2", "Correlation")),  digits = 2, caption="Correlations among Predictors")
```

|      | Variable 1                 | Variable 2                   | Correlation |
|:-----|:---------------------------|:-----------------------------|------------:|
| 702  | kw_max_min                 | kw_avg_min                   |        0.98 |
| 215  | n_unique_tokens            | n_non_stop_unique_tokens     |        0.91 |
| 2214 | rate_positive_words        | rate_negative_words          |       -0.90 |
| 1026 | kw_max_avg                 | kw_avg_avg                   |        0.88 |
| 1188 | self_reference_max_shares  | self_reference_avg_sharess   |        0.87 |
| 807  | kw_min_min                 | kw_max_max                   |       -0.86 |
| 1187 | self_reference_min_shares  | self_reference_avg_sharess   |        0.81 |
| 2213 | global_rate_negative_words | rate_negative_words          |        0.79 |
| 1620 | weekday_is_sunday          | is_weekend                   |        0.75 |
| 2484 | avg_negative_polarity      | min_negative_polarity        |        0.75 |
| 2158 | global_sentiment_polarity  | rate_positive_words          |        0.73 |
| 481  | n_non_stop_words           | average_token_length         |        0.73 |
| 2752 | title_subjectivity         | abs_title_sentiment_polarity |        0.72 |
| 2211 | global_sentiment_polarity  | rate_negative_words          |       -0.72 |
| 108  | n_tokens_content           | n_unique_tokens              |       -0.72 |
| 2160 | global_rate_negative_words | rate_positive_words          |       -0.70 |
| 864  | kw_max_max                 | kw_avg_max                   |        0.67 |
| 860  | kw_min_min                 | kw_avg_max                   |       -0.65 |
| 1887 | LDA_00                     | LDA_04                       |       -0.64 |
| 1619 | weekday_is_saturday        | is_weekend                   |        0.63 |
| 2699 | title_subjectivity         | abs_title_subjectivity       |       -0.59 |
| 2052 | global_sentiment_polarity  | global_rate_positive_words   |        0.59 |
| 267  | n_tokens_content           | num_hrefs                    |        0.58 |
| 2159 | global_rate_positive_words | rate_positive_words          |        0.57 |
| 2375 | avg_positive_polarity      | max_positive_polarity        |        0.57 |
| 214  | n_tokens_content           | n_non_stop_unique_tokens     |       -0.56 |
| 2537 | avg_negative_polarity      | max_negative_polarity        |        0.54 |
| 967  | kw_max_min                 | kw_max_avg                   |        0.53 |
| 2212 | global_rate_positive_words | rate_negative_words          |       -0.52 |
| 968  | kw_avg_min                 | kw_max_avg                   |        0.51 |
| 2753 | title_sentiment_polarity   | abs_title_sentiment_polarity |        0.51 |
| 2105 | global_sentiment_polarity  | global_rate_negative_words   |       -0.50 |
| 2754 | abs_title_subjectivity     | abs_title_sentiment_polarity |       -0.50 |

Correlations among Predictors

``` r
    corrplot(corMat2, is.corr=FALSE, tl.col="black", na.label=" ") #plot  correlations absolute value above 0.5
```

![](Project2Code_files/figure-gfm/correlation-1.png)<!-- -->

``` r
#weekday-shares relationshinship

dayData<-channelData%>%select(shares,starts_with("weekday_is"))%>%
    mutate(day_of_week=factor(case_when(as.logical(weekday_is_monday)~"Monday",
                             as.logical(weekday_is_tuesday)~"Tuesday",
                             as.logical(weekday_is_wednesday)~"Wednesday",
                             as.logical(weekday_is_thursday)~"Thursday",
                             as.logical(weekday_is_friday)~"Friday",
                             as.logical(weekday_is_saturday)~"Saturday",
                             as.logical(weekday_is_sunday)~"Sunday"),ordered=TRUE,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))%>%
  select(-starts_with("weekday_is"))%>%
  group_by(day_of_week)%>%
    dplyr::summarize(avg_Shares=mean(shares, na.rm=TRUE), n_articles=n())

ggplot(dayData, aes(x = day_of_week, y = avg_Shares)) + geom_bar(stat="identity",color = "lightgreen", aes(fill = day_of_week) )+
  scale_fill_grey()+ guides(fill="none")
```

![](Project2Code_files/figure-gfm/weekday%20summary-1.png)<!-- -->

``` r
#variables with highest correlation to shares
knitr::kable(slice(share_cor,1:10), digits=3, caption="10 variable most highly correlated with shares" , col.names=c("variable", "correlation with shares"))
```

| variable                   | correlation with shares |
|:---------------------------|------------------------:|
| self_reference_min_shares  |                   0.111 |
| self_reference_avg_sharess |                   0.105 |
| kw_avg_avg                 |                   0.087 |
| self_reference_max_shares  |                   0.076 |
| LDA_03                     |                   0.064 |
| kw_max_avg                 |                   0.055 |
| num_videos                 |                   0.050 |
| global_subjectivity        |                   0.048 |
| num_hrefs                  |                   0.041 |
| kw_avg_min                 |                   0.038 |

10 variable most highly correlated with shares

### Plots

## 4. Modeling

We first split the data for each channel into 70% for testing and 30%
for training.

``` r
#use set.seed for reproducibility
set.seed(101)

trainIndex <-caret::createDataPartition(channelData$shares, p = 0.7, list = FALSE)

training <-channelData[trainIndex,]
testing <-channelData[-trainIndex,]
```

For all models we will use 10-fold cross-validation repeated 5 times for
each type of model. Using the caret package, we set up the “control”
that provides for this cross-validation.

``` r
#We will use repeated 10-fold cv for all predictive techniques 

controlObject<-trainControl(method = "repeatedcv", 
                        number = 10, 
                        repeats=5)
```

``` r
#remove predictors with near zero variance (including indicators for other channels)

# check how much variance there is for all variables 
#nzv <- nearZeroVar(channelData, saveMetrics= TRUE)

#removed 8 variables with low variance

# Get variables with zero or near-zero variance and remove them from the dataframe
nzv <- nearZeroVar(channelData)
filteredChannelData <- channelData[, -nzv]
```

### Linear Models

#### Linear Regression Model 1

#### Linear Regression Model 2

Performed forward step-wise regression. Excluded variable that was found
to have zero variance. Terrible results.

``` r
#Note: Need to remove one day of week and 

set.seed(102)

# clust <- parallel::makePSOCKcluster(5)
# doParallel::registerDoParallel(clust)
# 
# lm2fit <- train(
#   shares~.,
#   data = filteredChannelData,
#   trControl= controlObject,
#   method= "lmStepAIC",
#   direction = "backward",
#   trace = FALSE,
#   preProcess =c("center","scale"),
#   na.action = na.omit,
#   verbose = FALSE)
# 
# predictionLm2 <- predict(lm2fit,newdata = testing)
# #Get RMSE value
# lm2_RMSE<-postResample(predictionLm2, obs = testing$shares)["RMSE"][[1]]
```

### Random Forest Model

### Boosted Tree Model

``` r
# formula using all variables other than those that have zero variance

# BTformula<-as.formula("shares~.")
# 
# set.seed(102)
# 
# tunegridBT<-expand.grid(n.trees=c(25, 50, 100, 150, 200), 
#                            interaction.depth=1:4, 
#                            shrinkage=0.1, 
#                            n.minobsinnode=10)
# 
# BTfit <- train(
#   form = BTformula,
#   data = filteredChannelData,
#   trControl= controlObject,
#   method="gbm",
#   tuneGrid=tunegridBT,
#   preProcess =c("center","scale"),
#   na.action = na.omit,
#   verbose = FALSE)
# 
# predictionBT <- predict(BTfit,newdata = testing)
# postResample(predictionBT, obs = filteredChannelData$shares)
```

## Model Comparisons

We calculate the RMSE for each model using the testing data. The winning
model will be the one with lowest RMSE.

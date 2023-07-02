Project2
================
Landon Batts, Jose Singer-Freeman
2023-07-02

## Data Wrangling

``` r
#Get data
raw_data<-read.csv("~/ST558/Project2/OnlineNewsPopularity.csv")

# Remove variables we won't use
newsData<-raw_data %>% select(-c(url,timedelta))


#Filter data from a single channel (just as test) 

#store business channel the global channel paramerter 
bindingIsLocked("params", env = .GlobalEnv)
```

    ## [1] TRUE

``` r
unlockBinding("params", env = .GlobalEnv)
params$channel<-"data_channel_is_bus"

# make the parameter as a name and then filter values where the variable is 1

busChannelData<-newsData %>% filter(eval(as.name(params$channel))==1)
```

# Basic Summary Statistics

# Modeling

## Linear Models

### Linear Regression Model 1

### Linear Regression Model 2

## Random Forest Model

## Boosted Tree Model

# project-1
This is a repo for project 1
---
title: "Interacting with APIs: Example with finacial data"
author: "Ningning Zhang"
date: "10/4/2021"
output: html_document

---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```  

```{r eval=FALSE}
rmarkdown::render(input="C:/Users/info/OneDrive/Desktop/U Drive-01Feb2019/NZ/ST558/Project-1/project-1", output_format = github_document)
```


## Load in packages

```{r echo=TRUE, warning=FALSE, message=FALSE, error=FALSE}
# load in the packages. 
library(tidyverse)
library(jsonlite)
library(cowplot)
library(imager)
library(broom)
library(RCurl)
library(httr)
```   


## API interaction functions  


### Here we drew the stock price of APPLE from 1999-01-01 to 2021-09-30 from website [polygon] (https://polygon.io/docs/getting-started). The users may update the date and ticker names, range.  

```{r Apple stock price}
#get data from API
apple <- GET("https://api.polygon.io/v2/aggs/ticker/AAPL/range/1/day/1999-01-01/2021-09-30?apiKey=5xypYpHq6qD3InDjLOiwTM16yogYvmgE")
applStock <-apple$content %>% rawToChar() %>% fromJSON()
applStock_df <- as.data.frame(applStock)
#drop columns
applStock_df$queryCount <-NULL  
applStock_df$resultsCount <-NULL
applStock_df$adjusted <-NULL
applStock_df$status <-NULL
applStock_df$request_id <-NULL
applStock_df$count <-NULL
applStock_df$results.n <-NULL
applStock_df$results.t <-NULL
#rename columns
colnames(applStock_df) <- c("tiker", "volume", "v.weighted avg price", "open", "close", "highest", "lowest")

```  

### Here we drew the stock price of Facebook from 1999-01-01 to 2021-09-30.  

```{r facebook stock price}

#get data from API
fb <- GET("https://api.polygon.io/v2/aggs/ticker/FB/range/1/day/1999-01-01/2021-09-30?apiKey=5xypYpHq6qD3InDjLOiwTM16yogYvmgE")
fbStock <-fb$content %>% rawToChar() %>% fromJSON()
fbStock_df <- as.data.frame(fbStock)
#drop columns
fbStock_df$queryCount <-NULL
fbStock_df$resultsCount <-NULL
fbStock_df$adjusted <-NULL
fbStock_df$status <-NULL
fbStock_df$request_id <-NULL
fbStock_df$count <-NULL
fbStock_df$results.n <-NULL
fbStock_df$results.t <-NULL
#rename columns
colnames(fbStock_df) <- c("tiker", "volume", "v.weighted avg price", "open", "close", "highest", "lowest")

```    

## Data exploration  

Now that we can interact with the two endpoints of financial data.
create a contingency table. but I think the data here is numeric, not categorical data.

```{r table for Apple stock highest price from 1999-01-01 to 2021-09-30}

tabAPPL <-data.frame(table(applStock_df$tiker, applStock_df$highest))
colnames(tabAPPL) <- c("Tiker", "Highest price", "Frequency")

```  

```{r table for FB stock highest price from 1999-01-01 to 2021-09-30}
tabFB <-data.frame(table(fbStock_df$tiker, fbStock_df$highest))
colnames(tabFB) <- c("Tiker", "Highest price", "Frequency")
```  


### Create numeric tables  

```{r numeric tables for Apple stock}
summary(applStock_df$`v.weighted avg price`)
var(applStock_df$`v.weighted avg price`)
sd(applStock_df$`v.weighted avg price`)
cor(applStock_df$`v.weighted avg price`, applStock_df$volume)## no linear relationship between Apple stock volume and price.

```  


```{r numeric tables for FB stock}
summary(fbStock_df$`v.weighted avg price`)
var(fbStock_df$`v.weighted avg price`)
sd(fbStock_df$`v.weighted avg price`)
cor(fbStock_df$`v.weighted avg price`, fbStock_df$volume)## no linear relationship between Facebook stock volume and price.
```


### Create plots for the two endpoints from financial data.  
I am curious about the difference between highest and lowest price (diff1), also the difference between close and open price (diff2).  

```{r Apple stock price difference}  
diff1_APPL <- applStock_df %>% mutate(diff1=abs(highest-lowest))
applStock_df <- diff1_APPL %>% mutate(diff2=abs(close-open))
```  


```{r Facebook stock price difference}
diff1_FB <- fbStock_df %>% mutate(diff1=abs(highest-lowest))
fbStock_df <- diff1_FB %>% mutate(diff2=abs(close-open))
```  


### Create the histogram for Apple stock and FB stock prices.

From the two histograms, we can see the price differences for Apple and FB between highest and lowest, and open and close are getting smaller and smaller from 1999-01-01 to 2021-09-30, which means we can make a market call at anytime in one day currently.  


Make a histogram of Apple stock absolute difference between highest and lowest price.


```{r histogram of Apple stock difference between highest and lowest price}
hist(applStock_df$diff1, main = paste("Histogram of Apple stock price difference between highest and lowest"), xlab = "|diff1|", col = "blue")
```  


Make a histogram of Apple stock absolute difference between open and close price.


```{r histogram of Apple stock difference between open and close price}
hist(applStock_df$diff2, main = paste("Histogram of Apple stock price difference between open and close"), xlab = "|diff2|", col = "green")
```  


Make a histogram of FB stock absolute difference between highest and lowest price.


```{r histogram of FB stock difference between highest and lowest price}
hist(fbStock_df$diff1, main = paste("Histogram of FB stock price difference between highest and lowest"), xlab = "|diff1|", col = "blue")
```  


Make a histogram of FB stock absolute difference between open and close price.


```{r  histogram of FB stock difference between open and closeprice}
hist(fbStock_df$diff2, main = paste("Histogram of FB stock price difference between open and close"), xlab = "|diff2|", col = "green")
```  


### Create the scatter plot for Apple stock and FB stock prices between diff1 and volume.  

From the two scatter plots of Apple and FB stock price differences (|diff1|) between highest and lowest by volume, we can see there is a pretty good linear relationship between absolute value of diff1 (|diff1|) and volume. with the stock volume getting bigger, the stock differences for Apple and FB between highest and lowest are getting higher. 

correlation_Appl <-cor(applStock_df$diff1, applStock_df$volume)
correlation_FB <-cor(fbStock_df$diff1, fbStock_df$volume)


```{r scatter plot of Apple stock price difference between highest and lowest by volume} 

plot2_APPL <- ggplot(applStock_df, aes(x= diff1, y=volume)) +
               geom_point() + 
               geom_smooth() + 
               geom_smooth(method = lm, col = "Red") + 
               ggtitle("Apple stock price difference between highest and lowest by volume")



plot2_APPL
```  


```{r scatter plot of FB stock price difference between highest and lowest by volume}

plot2_FB <- ggplot(fbStock_df, aes(x= diff1, y=volume)) +
               geom_point() + 
               geom_smooth() + 
               geom_smooth(method = lm, col = "purple") + 
               ggtitle("FB stock price difference between highest and lowest by volume")



plot2_FB
```  


### Create the box plots for Apple stock and FB stock highest and lowest prices. 

From the two box plots, we can see the median value of highest price for Apple is around "115$"; and the max value is around 160$, and the median value of lowest price for Apple is around 115$, and the max value is around 160$ from 1999-01-01 to 2021-09-30. 

The median value of highest price for FB is around 260$, and the maximum value is around 380$, and the median value of lowest price for FB is around 260$, and the maximum value is around 380$ from 1999-01-01 to 2021-09-30. 


```{r boxplot for Apple stock highest and lowest prices}
plot3_APPL <- ggplot(applStock_df, aes(x= highest)) +
               geom_boxplot(fill="grey") +
               ggtitle("Apple stock highest price")


plot4_APPL <- ggplot(applStock_df, aes(x= lowest)) +
               geom_boxplot(fill="yellow") + 
               ggtitle("Apple stock lowest price")

plot3_APPL



plot4_APPL
```  



```{r boxplot for FB stock highest and lowest prices}

plot3_FB <- ggplot(fbStock_df, aes(x= highest)) +
               geom_boxplot(fill="grey") +
               ggtitle("FB stock highest price")


plot4_FB <- ggplot(fbStock_df, aes(x= lowest)) +
               geom_boxplot(fill="yellow") + 
               ggtitle("FB stock lowest price")

plot3_FB 



plot4_FB 
```    


## Wrap-up  

For everything I did in this vignette, I built functions to interact with endpoints from the financial data API, retrieved some of the data, and explored it using tables, numerical summaries and data visualization. I found some interesting things, the stock price differences between open and close, highest and lowest in one day are getting smaller with the time going by, which implies we may purchase our stock at anytime in one day. Also, the stock prices for Apple and Facebook are really getting higher and higher with the time going by.


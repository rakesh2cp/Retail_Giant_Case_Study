############################################################################################
# Sales Forecasting solution R Code for Forecasting the Sales for Retail Giant  based on   #
# past data set. The Analysis is done via Time Series with the provided dataset.           #
# Graphs and charts are prepared for different analysis                                    #
# Please open this file in RStudio for better code readability                             #
#                                                                                          #
# Author: Rakesh Pattanaik.                                                                #   
############################################################################################

# Business Understanding:----
# Sales forecasting is the process of estimating future sales. 
# Accurate sales forecasts enable companies to make informed business decisions 
# and predict short-term and long-term performance. 
# Projection of achievable sales revenue, based on historical sales data, 
# analysis of market surveys and trends, and salespersons' estimates. 
# It forms the basis of a business plan because the level of sales revenue 
# affects practically every aspect of a business. The amount by which the average sales 
# volume of a company's products or services has grown, typically from year to year.  
# In this case study we will analyse the supplied data set with time series in order to 
# forecast the sales as well as quantity for next six month studying the dataset provided for 24 months.

# Install Required Library -----
# Please install the below libraries before running the code.
# The packages need to be installed once and hence purposefully 
# kept commented after installing the packages once in order to avoid 
# reinstalling packages again and again.
# Please uncomment and install the packages if these packages 
# dont exist on your machine.
#install.packages("stringr")	
#install.packages("dplyr")	
#install.packages("tidyr")
#install.packages("ggplot2")	
#install.packages("cowplot")
#install.packages("lubridate")
#install.packages("forecast")
#install.packages("tseries")

# Import the required Library -----
# Please clear all the object and plots from the R-studio environment before running the code.
library(stringr)
library(dplyr)	
library(tidyr)
library(ggplot2)
library(cowplot)
library(lubridate)
library(forecast)
library(tseries)


# Data Understanding-----
# The supplied data set is present in Global Superstore.csv file.
# file data has transaction level data, where each row represents a particular 
# order made on the online store.

# Data Cleaning & Preparation----
# Set Working Directory.
# The working directory may change depending on the computer/machine the code is running. 
# Please change the working directory as per your requirement and where
# all the data files (The provided data file in the assignment) are present.

setwd("/Users/hduser/Desktop/Upgrad/time_series/case_study/")

#Please make sure all the data files are listed in the directory
list.files()


# Importing the given files as a dataframe in R. 
# Making sure that strings are not treated as factors and blanks are also treated as NA values.

global_superstore <- read.csv ("Global Superstore.csv", header = TRUE, fill = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA"))

#Checking for duplicate rows. None found
global_superstore <- unique(global_superstore)


#Checking for NA values. 
sum(is.na(global_superstore))

colnames(global_superstore)[colSums(is.na(global_superstore)) > 0]

#its observed there are 41296 NA values in the dataset. All the NA values are in  the column Postal Code
#for our analysis this is not a critical input and hence we are not doing NA value treatment for it.


#Displaying structure of the global_superstore dataframe
str(global_superstore)

#for our analysis, the variables that are needed are as follows:

#1. Order.Date : Date on which the order was placed. This will be the time variable
#2. Segment : The market segment to which the product belongs (total 3)
#3. Market : Market segment to which the customer belongs (total 7)
#4. Sales : Total sales value of the transaction
#5. Quantity : Quantity of the product ordered
#6. Profit : Profit made on the transaction


#making a subset with the desired vaiables
gs_df = subset(global_superstore, select = c(Order.Date, Segment, Market, Sales, Quantity, Profit))


#Displaying structure of the gs_df dataframe
str(gs_df)

#It is observed that the variable Order.Date is of character type and need to be converted to date type.
#Categorical vairable Segment & Market are also of character type and need to be converted to factor type.

gs_df$Order.Date <- as.Date(gs_df$Order.Date, format = "%d-%m-%Y")

gs_df$Segment <- as.factor(gs_df$Segment)

gs_df$Market <- as.factor(gs_df$Market)


# Doing EDA on the above dataset----

# Cloning the Dataset for Plotting
gs_df_plt <- gs_df

# Deriving Year from the Order Date column
gs_df_plt$Order.Year <-format(as.Date(gs_df$Order.Date, format="%d/%m/%Y"),"%Y")

# Deriving Month from the Order Date column
gs_df_plt$Order.Month <-format(as.Date(gs_df$Order.Date, format="%d/%m/%Y"),"%m")

# Setting Theme for Charts
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=9), 
                   legend.position="right",axis.title=element_text(size=10,face="bold.italic"))

# Plot the Quantity, Sales, profit for Market and Segment.
plot_grid(ggplot(gs_df_plt, aes(x=Market,fill=Quantity))+ geom_bar(fill = "#FF6666")+bar_theme1+ylab("Qnty") + ggtitle("Quantity_Market_wise: Jan'11-Dec'14"), 
          ggplot(gs_df_plt, aes(x=Market,fill=Sales))+ geom_bar(fill = "#FF0000")+bar_theme1+ylab("Sales") + ggtitle("Sales_Market_wise: Jan'11-Dec'14"), 
          ggplot(gs_df_plt, aes(x=Market,fill=Profit))+ geom_bar(fill = "#3399FF")+bar_theme1+ylab("Profit") + ggtitle("Profit_Market_wise: Jan'11-Dec'14"), 
          ggplot(gs_df_plt, aes(x=Segment,fill=Quantity))+ geom_bar(fill = "#33CCFF")+bar_theme1+ylab("Qnty") + ggtitle("Quantity_Segment_wise: Jan'11-Dec'14"), 
          ggplot(gs_df_plt, aes(x=Segment,fill=Sales))+ geom_bar(fill = "#FF4444")+bar_theme1+ylab("Sales") + ggtitle("Sales_Segment_wise: Jan'11-Dec'14"), 
          ggplot(gs_df_plt, aes(x=Segment,fill=Profit))+ geom_bar(fill = "#FF3399")+bar_theme1+ylab("Profit") + ggtitle("Profit_Segment_wise: Jan'11-Dec'14"), 
          align = "h" ) 


# Plot the Year Wise Quantity, Sales, profit for Market and Segment.
plot_grid(ggplot(gs_df_plt, aes(x=Order.Year,fill=Quantity))+ geom_bar(fill = "#00FFFF")+bar_theme1+xlab("Year")+ylab("Odr_Qnty") + ggtitle("Quantity sold year wise") , 
          ggplot(gs_df_plt, aes(x=Order.Year,fill=Sales))+ geom_bar(fill = "#33CCFF")+bar_theme1+xlab("Year")+ylab("Sales") + ggtitle("Sales achieved year wise"), 
          ggplot(gs_df_plt, aes(x=Order.Year,fill=Profit))+ geom_bar(fill = "#0066FF")+bar_theme1+xlab("Year")+ylab("Profit") + ggtitle("Profit generated year wise"), 
          align = "h")

# Plot the Month wise Quantity & Sales for Market and Segment
plot_profit<-ggplot(gs_df_plt,aes(x=Order.Year,y=Quantity,group=1))
plot_profit+geom_point()+geom_line()+facet_grid(Market~Segment)

plot_profit<-ggplot(gs_df_plt,aes(x=Order.Year,y=Sales,group=1))
plot_profit+geom_point()+geom_line()+facet_grid(Market~Segment)

# Creating 21 subsets (3 type of Segment * 7 type of Market)  
gs_split <- split(gs_df,list(gs_df$Segment, gs_df$Market))


#creating 21 lists to hold monthly aggregated values for Sales, Quantity & Profit  
gs_agg <- vector("list", length = 21)

#creating dataframe 'cv' to hold coefficient of variation of the Profit for all 21 market segments
cv <- data.frame(cv=double())

#Aggregating Sales, Quantity & Profit, over the Order Date to arrive at year wise monthly values for all 21 market segments

for(i in 1:21)
  {
  year_month <- strftime(gs_split[[i]]$Order.Date, "%Y-%m") #extracting year and month values
  
  gs_agg[[i]] <- aggregate(cbind(Sales, Quantity, Profit)~year_month, data=gs_split[[i]], FUN=sum)
  
  cv[i,1] <- sd(gs_agg[[i]]$Profit)/mean(gs_agg[[i]]$Profit) #calculating coefficient of variation for Profit
  }

#Appending the market & segment information to the dataframe 'cv'
cv$market_segment <- names(gs_split)

# Plotting for CV for Market Segment
ggplot(cv, aes(market_segment,cv))+ geom_point(aes(colour = factor(market_segment)))+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=9), 
                                                                                           legend.position="right",axis.title=element_text(size=10,face="bold.italic"))

# Sorting the dataframe 'cv' based on ascending order of coefficient of variation of the Profit
cv[order(cv$cv),]

# Its observed that the following two market segments have lowest coefficient of variation of the Profit
# 1. Consumer.EU   : gs_agg[[13]]
# 2. Consumer.APAC : gs_agg[[4]]
# These are the most profitable and consistently profitable market segments that we will use for model building

# Making a subset with the desired market segments
gs_con_EU = as.data.frame(gs_agg[[13]])

gs_con_APAC = as.data.frame(gs_agg[[4]])


#converting the Year_month to sequence of month for above datasetes
gs_con_EU$year_month <- seq(1:nrow(gs_con_EU))
colnames(gs_con_EU)[1] <- "Month"

gs_con_APAC$year_month <- seq(1:nrow(gs_con_APAC))
colnames(gs_con_APAC)[1] <- "Month"


#splitting above datasets into training and validation (last 6 months) and keeping only Sales & Quantity values

gs_con_EU_train <- gs_con_EU[1:42,-4]

gs_con_EU_valid <- gs_con_EU[43:48,-4]


gs_con_APAC_train <- gs_con_APAC[1:42,-4]

gs_con_APAC_valid <- gs_con_APAC[43:48,-4]


#Model Building & Evaluation-----

#There are 2 dependent variables Sales & Quantity, we will be building seperate model for each

#Model 1 : For Consumer.EU Sales - we will be using the training dataset for model building----

#Converting the sales data into a timeseries

ts_EU_train_sales <- ts(gs_con_EU_train$Sales)

#Plotting the above timeseries

plot(ts_EU_train_sales, main="EU Sales : Jan'11 - Dec'14", xlab = "Month", ylab = "Sales", col="black")


#Smoothing the timeseries using moving average method

w <- 3

smoothedseries <- stats::filter(ts_EU_train_sales, filter=rep(1/(2*w+1), (2*w+1)), method="convolution", sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]

for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(ts_EU_train_sales)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}


#Plot the smoothed time series

timevals_in <- gs_con_EU_train$Month

lines(smoothedseries, col="blue", lwd=2)

#Based on the above, we see that for w=3, we are getting decent smoothing and also not loosing out on much of the information
#We also checked for other w vaues from 1 to 10 and found that for w=3, we got the best fit
#Hence, for building our model, we will be using smoothed series with w=3


#Model 1A : Model building via Classical Decomposition----

#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function and trend with a polynomial of order 3

lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3) + Month, data=smootheddf)

summary(lmfit)
#Its observed the Adjusted R-squared value is 0.9671, which is pretty decent, hence we will continue with this model.

global_pred <- predict(lmfit, Month=timevals_in)

summary(global_pred)

lines(timevals_in, global_pred, col='red', lwd=2)

#Its observed global component(red curve) follows the smoothed series(blue curve) pretty well
#Hence, with this model we have reasonably captured the global trend and seasonality


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- ts_EU_train_sales - global_pred
plot(local_pred, col='red', type = "l")

#Its observed that the local component is without and visible pattern
#Now we will check if this is just noise or is there any auto-regressive part present in it

#Plotting ACF & PACF to test is there is any auto-regressive part present in the local component

acf(local_pred)
acf(local_pred, type="partial")
#Its observed that bot ACF & PACF show that for non-zero lag, the correlation values are between the confidence intervals (dotted blue line)
#Hence this local component is only having white noise with no auto-regressive part in it.


#Re-confriming the above observation via the auto.arima model

armafit <- auto.arima(local_pred)

par(mar=c(2,2,2,2))
tsdiag(armafit)

armafit

#Its observed that the ARIMA model for the local component comes out to be AR(0,0,0)
#Hence this local component is only having white noise with no auto-regressive part in it.
#Further, the log likelihood=-448.09, AIC=898.17, AICc=898.27 and BIC=899.91


#We'll check if the residual series is indeed white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
#p-value is 0.01 < 0.05 and hence we will reject the null hypthesis
#The Dickey-Fuller test states that the residual series is stationary

kpss.test(resi)
#p-value is 0.1 > 0.05 and hence we will not reject the null hypthesis 
#The KPSS test states that the residual series is stationary

#Both the ADF test and the KPSS test states that the series is strongly stationary.
#Hence the residual series is a white noise

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- gs_con_EU_valid$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,gs_con_EU_valid$Sales)[5]
MAPE_class_dec
#The MAPE value comes out to be 23.99% which is not too high and our model is doing decent forecasting

#Let's also plot the predictions along with original values, to get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))

total_timeser <- ts(gs_con_EU$Sales)

plot(total_timeser, col = "black",main="Classical Decom. EU Sales")
lines(class_dec_pred, col = "red")

#Its observed that the predicted values from our model closely follow the original timeseries


#Model 1B : Model building via auto ARIMA-----

autoarima <- auto.arima(ts_EU_train_sales)
autoarima
#Its observed that the auto.arima function has fit the ARIMA(2,1,0) model on the data
#The 1 in the middle signifies that one level of differencning was done, which evidences the presence of trend in the data

#Further, the log likelihood=-445.84, AIC=897.67, AICc=898.32 and BIC=902.81
#These values are pretty similar to what we obtained via the classical decompositon method

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
#Its observed that the auto arima model (red curve) follows the original timeseries but towards the end it deviates


#Again, let's check if the residual series is white noise

resi_auto_arima <- ts_EU_train_sales - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#p-value is 0.01 < 0.05 and hence we will reject the null hypthesis
#The Dickey-Fuller test states that the residual series is stationary

kpss.test(resi_auto_arima)
#p-value is 0.1 > 0.05 and hence we will not reject the null hypthesis 
#The KPSS test states that the residual series is stationary


#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred, gs_con_EU_valid[,2])[5]
MAPE_auto_arima

#The MAPE value comes out to be 28.92% which approx 5%  then what we achieved via the classical decomposition
#Thus, the clasical decomposition model is slightly better then the auto arima in forecasting


#Lastly, let's plot the predictions along with original values, to get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black",main="Auto ARIMA EU Sales")
lines(auto_arima_pred, col = "red")

#Its observed that the predicted values from the auto arima model follow the original timeseries


#Model 2 : For Consumer.EU Quantity - we will be using the training dataset for model building----

#Converting the quantity data into a timeseries

ts_EU_train_quantity <- ts(gs_con_EU_train$Quantity)

#Plotting the above timeseries

plot(ts_EU_train_quantity, main="EU Quantity : Jan'11 - Dec'14", xlab = "Month", ylab = "Quantity", col="black")


#Smoothing the timeseries using moving average method

w <- 3

smoothedseries <- stats::filter(ts_EU_train_quantity, filter=rep(1/(2*w+1), (2*w+1)), method="convolution", sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]

for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(ts_EU_train_quantity)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}


#Plot the smoothed time series

timevals_in <- gs_con_EU_train$Month

lines(smoothedseries, col="blue", lwd=2)

#Based on the above, we see that for w=3, we are getting decent smoothing and also not loosing out on much of the information
#We also checked for other w vaues from 1 to 10 and found that for w=3, we got the best fit
#Hence, for building our model, we will be using smoothed series with w=3


#Model 2A : Model building via Classical Decomposition----

#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quanity')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function and trend with a polynomial of order 3

lmfit <- lm(Quanity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3) + Month, data=smootheddf)

summary(lmfit)
#Its observed the Adjusted R-squared value is 0.9676, which is pretty decent, hence we will continue with this model.

global_pred <- predict(lmfit, Month=timevals_in)

summary(global_pred)

lines(timevals_in, global_pred, col='red', lwd=2)

#Its observed global component(red curve) follows the smoothed series(blue curve) pretty well
#Hence, with this model we have reasonably captured the global trend and seasonality


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- ts_EU_train_quantity - global_pred
plot(local_pred, col='red', type = "l")

#Its observed that the local component is without and visible pattern
#Now we will check if this is just noise or is there any auto-regressive part present in it

#Plotting ACF & PACF to test is there is any auto-regressive part present in the local component

acf(local_pred)
acf(local_pred, type="partial")
#Its observed that bot ACF & PACF show that for non-zero lag, the correlation values are between the confidence intervals (dotted blue line)
#Hence this local component is only having white noise with no auto-regressive part in it.


#Re-confriming the above observation via the auto.arima model

armafit <- auto.arima(local_pred)

par(mar=c(2,2,2,2))
tsdiag(armafit)

armafit

#Its observed that the ARIMA model for the local component comes out to be AR(0,0,0)
#Hence this local component is only having white noise with no auto-regressive part in it.
#Further, the log likelihood=-262.21, AIC=526.42, AICc=526.52 and BIC=528.16


#We'll check if the residual series is indeed white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
#p-value is 0.01 < 0.05 and hence we will reject the null hypthesis
#The Dickey-Fuller test states that the residual series is stationary

kpss.test(resi)
#p-value is 0.1 > 0.05 and hence we will not reject the null hypthesis 
#The KPSS test states that the residual series is stationary

#Both the ADF test and the KPSS test states that the series is strongly stationary.
#Hence the residual series is a white noise

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- gs_con_EU_valid$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,gs_con_EU_valid$Quantity)[5]
MAPE_class_dec
#The MAPE value comes out to be 35.81% which is not too high and our model is doing decent forecasting

#Let's also plot the predictions along with original values, to get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))

total_timeser <- ts(gs_con_EU$Quantity)

plot(total_timeser, col = "black",main="Classical Decom. EU_Quantity")
lines(class_dec_pred, col = "red")

#Its observed that the predicted values from our model closely follow the original timeseries


#Model 2B : Model building via auto ARIMA-----

autoarima <- auto.arima(ts_EU_train_quantity)
autoarima
#Its observed that the auto.arima function has fit the ARIMA(2,1,0) model on the data
#The 1 in the middle signifies that one level of differencning was done, which evidences the presence of trend in the data

#Further, the log likelihood=-261.9, AIC=529.8, AICc=530.44 and BIC=534.94
#These values are pretty similar to what we obtained via the classical decompositon method

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
#Its observed that the auto arima model (red curve) follows the original timeseries 


#Again, let's check if the residual series is white noise

resi_auto_arima <- ts_EU_train_quantity - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#p-value is 0.045 < 0.05 and hence we will reject the null hypthesis
#The Dickey-Fuller test states that the residual series is stationary

kpss.test(resi_auto_arima)
#p-value is 0.1 > 0.05 and hence we will not reject the null hypthesis 
#The KPSS test states that the residual series is stationary


#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred, gs_con_EU_valid[,3])[5]
MAPE_auto_arima

#The MAPE value comes out to be 30.13% which approx 5% lower then what we achieved via the classical decomposition
#Thus, the auto arima model is slightly better then the clasical decomposition forecasting


#Lastly, let's plot the predictions along with original values, to get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black",main="Auto ARIMA EU_Quantity")
lines(auto_arima_pred, col = "red")

#Its observed that the predicted values from the auto arima model follow the original timeseries



#Model 3 : For Consumer.APAC Sales - we will be using the training dataset for model building----

#Converting the sales data into a timeseries

ts_APAC_train_sales <- ts(gs_con_APAC_train$Sales)

#Plotting the above timeseries

plot(ts_APAC_train_sales, main="APAC Sales : Jan'11 - Dec'14", xlab = "Month", ylab = "Sales", col="black")


#Smoothing the timeseries using moving average method

w <- 3

smoothedseries <- stats::filter(ts_APAC_train_sales, filter=rep(1/(2*w+1), (2*w+1)), method="convolution", sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]


for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(ts_APAC_train_sales)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}


#Plot the smoothed time series

timevals_in <- gs_con_APAC_train$Month

lines(smoothedseries, col="blue", lwd=2)

#Based on the above, we see that for w=3, we are getting decent smoothing and also not loosing out on much of the information
#We also checked for other w vaues from 1 to 10 and found that for w=3, we got the best fit
#Hence, for building our model, we will be using smoothed series with w=3


#Model 3A : Model building via Classical Decomposition----

#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function and trend with a polynomial of order 3

lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3) + Month, data=smootheddf)

summary(lmfit)
#Its observed the Adjusted R-squared value is  0.9827, which is pretty decent, hence we will continue with this model.

global_pred <- predict(lmfit, Month=timevals_in)

summary(global_pred)

lines(timevals_in, global_pred, col='red', lwd=2)

#Its observed global component(red curve) follows the smoothed series(blue curve) pretty well
#Hence, with this model we have reasonably captured the global trend and seasonality


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- ts_APAC_train_sales - global_pred
plot(local_pred, col='red', type = "l")

#Its observed that the local component is without and visible pattern
#Now we will check if this is just noise or is there any auto-regressive part present in it

#Plotting ACF & PACF to test is there is any auto-regressive part present in the local component

acf(local_pred)
acf(local_pred, type="partial")
#Its observed that bot ACF & PACF show that for non-zero lag, the correlation values are between the confidence intervals (dotted blue line)
#Hence this local component is only having white noise with no auto-regressive part in it.


#Re-confriming the above observation via the auto.arima model

armafit <- auto.arima(local_pred)

par(mar=c(2,2,2,2))
tsdiag(armafit)

armafit

#Its observed that the ARIMA model for the local component comes out to be AR(0,0,0)
#Hence this local component is only having white noise with no auto-regressive part in it.
#Further, the log likelihood=-446.83, AIC=895.66, AICc=895.76 and BIC=987.4


#We'll check if the residual series is indeed white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
#p-value is 0.01 < 0.05 and hence we will reject the null hypthesis
#The Dickey-Fuller test states that the residual series is stationary

kpss.test(resi)
#p-value is 0.1 > 0.05 and hence we will not reject the null hypthesis 
#The KPSS test states that the residual series is stationary

#Both the ADF test and the KPSS test states that the series is strongly stationary.
#Hence the residual series is a white noise

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- gs_con_APAC_valid$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,gs_con_APAC_valid$Sales)[5]
MAPE_class_dec
#The MAPE value comes out to be 37.38% which is not too high and our model is doing decent forecasting

#Let's also plot the predictions along with original values, to get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))

total_timeser <- ts(gs_con_APAC$Sales)

plot(total_timeser, col = "black",main="Classical Decom. APAC_Sales")
lines(class_dec_pred, col = "red")

#Its observed that the predicted values from our model closely follow the original timeseries


#Model 3B : Model building via auto ARIMA-----

autoarima <- auto.arima(ts_APAC_train_sales)
autoarima
#Its observed that the auto.arima function has fit the ARIMA(0,1,1) model on the data
#The 1 in the middle signifies that one level of differencning was done, which evidences the presence of trend in the data

#Further, the log likelihood=-447.11, AIC=898.23, AICc=898.55 and BIC=901.66
#These values are pretty similar to what we obtained via the classical decompositon method

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
#Its observed that the auto arima model (red curve) follows the original timeseries 


#Again, let's check if the residual series is white noise

resi_auto_arima <- ts_APAC_train_sales - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#p-value is 0.01 < 0.05 and hence we will reject the null hypthesis
#The Dickey-Fuller test states that the residual series is stationary

kpss.test(resi_auto_arima)
#p-value is 0.1 > 0.05 and hence we will not reject the null hypthesis 
#The KPSS test states that the residual series is stationary


#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred, gs_con_APAC_valid[,2])[5]
MAPE_auto_arima

#The MAPE value comes out to be  27.68% which is approx 10% less then what we achieved via the classical decomposition
#Thus, the auto arima model is better then the clasical decomposition model in forecasting


#Lastly, let's plot the predictions along with original values, to get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black",main="Auto ARIMA APAC_SALES")
lines(auto_arima_pred, col = "red")

#Its observed that the predicted values from the auto arima model follow the original timeseries



#Model 4 : For Consumer.APAC Quantity - we will be using the training dataset for model building----

#Converting the quantity data into a timeseries

ts_APAC_train_quantity <- ts(gs_con_APAC_train$Quantity)

#Plotting the above timeseries

plot(ts_APAC_train_quantity, main="APAC Quantity : Jan'11 - Dec'14", xlab = "Month", ylab = "Quantity", col="black")


#Smoothing the timeseries using moving average method

w <- 3

smoothedseries <- stats::filter(ts_APAC_train_quantity, filter=rep(1/(2*w+1), (2*w+1)), method="convolution", sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]

for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(ts_APAC_train_quantity)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}


#Plot the smoothed time series

timevals_in <- gs_con_APAC_train$Month

lines(smoothedseries, col="blue", lwd=2)

#Based on the above, we see that for w=3, we are getting decent smoothing and also not loosing out on much of the information
#We also checked for other w vaues from 1 to 10 and found that for w=3, we got the best fit
#Hence, for building our model, we will be using smoothed series with w=3


#Model 4A : Model building via Classical Decomposition----

#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quanity')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function and trend with a polynomial of order 3

lmfit <- lm(Quanity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3) + Month, data=smootheddf)

summary(lmfit)
#Its observed the Adjusted R-squared value is 0.9799, which is pretty decent, hence we will continue with this model.

global_pred <- predict(lmfit, Month=timevals_in)

summary(global_pred)

lines(timevals_in, global_pred, col='red', lwd=2)

#Its observed global component(red curve) follows the smoothed series(blue curve) pretty well
#Hence, with this model we have reasonably captured the global trend and seasonality


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- ts_APAC_train_quantity - global_pred
plot(local_pred, col='red', type = "l")

#Its observed that the local component is without and visible pattern
#Now we will check if this is just noise or is there any auto-regressive part present in it

#Plotting ACF & PACF to test is there is any auto-regressive part present in the local component

acf(local_pred)
acf(local_pred, type="partial")
#Its observed that bot ACF & PACF show that for non-zero lag, the correlation values are between the confidence intervals (dotted blue line)
#Hence this local component is only having white noise with no auto-regressive part in it.


#Re-confriming the above observation via the auto.arima model

armafit <- auto.arima(local_pred)

par(mar=c(2,2,2,2))
tsdiag(armafit)

armafit

#Its observed that the ARIMA model for the local component comes out to be AR(0,0,0)
#Hence this local component is only having white noise with no auto-regressive part in it.
#Further, the log likelihood=-258.37, AIC=518.75, AICc=518.85 and BIC=520.48


#We'll check if the residual series is indeed white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
#p-value is 0.01 < 0.05 and hence we will reject the null hypthesis
#The Dickey-Fuller test states that the residual series is stationary

kpss.test(resi)
#p-value is 0.1 > 0.05 and hence we will not reject the null hypthesis 
#The KPSS test states that the residual series is stationary

#Both the ADF test and the KPSS test states that the series is strongly stationary.
#Hence the residual series is a white noise

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- gs_con_APAC_valid$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,gs_con_APAC_valid$Quantity)[5]
MAPE_class_dec
#The MAPE value comes out to be 30.49% which is not too high and our model is doing decent forecasting

#Let's also plot the predictions along with original values, to get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))

total_timeser <- ts(gs_con_EU$Quantity)

plot(total_timeser, col = "black",main="Classical Decom. APAC_Qnty")
lines(class_dec_pred, col = "red")

#Its observed that the predicted values from our model closely follow the original timeseries


#Model 4B : Model building via auto ARIMA-----

autoarima <- auto.arima(ts_APAC_train_quantity)
autoarima
#Its observed that the auto.arima function has fit the ARIMA(0,1,0) model on the data
#The 1 in the middle signifies that one level of differencning was done, which evidences the presence of trend in the data

#Further, the log likelihood=-266.07, AIC=534.14, AICc=532.24 and BIC=535.85
#These values are pretty similar to what we obtained via the classical decompositon method

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
#Its observed that the auto arima model (red curve) follows the original timeseries 


#Again, let's check if the residual series is white noise

resi_auto_arima <- ts_APAC_train_quantity - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#p-value is 0.01 < 0.05 and hence we will reject the null hypthesis
#The Dickey-Fuller test states that the residual series is stationary

kpss.test(resi_auto_arima)
#p-value is 0.1 > 0.05 and hence we will not reject the null hypthesis 
#The KPSS test states that the residual series is stationary


#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred, gs_con_APAC_valid[,3])[5]
MAPE_auto_arima

#The MAPE value comes out to be 26.24% which approx 4% lower then what we achieved via the classical decomposition
#Thus, the auto arima model is slightly better then the clasical decomposition model in forecasting


#Lastly, let's plot the predictions along with original values, to get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black",main = "AUTO ARIMA APAC_Qnty")
lines(auto_arima_pred, col = "red")

#Its observed that the predicted values from the auto arima model follow the original timeseries

#Recommendations----
# 1
# For both EU and APAC Consumer segment the sales shows increasing trend, 
# as per our model the forecasted sales for last six months is around 50,000 and  
# we recommend that the Global Mart’s online store’s site reliability/availability 
# parameters should be tuned to handle the increase in sales.

# 2
# For both EU and APAC Consumer segment the quantity shows increasing trend, 
# as per our model the forecasted quantity for last six months is around 600 and 
# we recommend that Global-Mart should stock inventory accordingly to meet the 
# increase in demand.

library(ggfortify)
library(scales)
library(tidyr)
library(tseries)
library(seasonal)
library(reshape2)
library(ggseas)
library(forecast)
library(zoo)
library(xts)
library(shiny)

#####About#####

#This file is for all time series analysis related to the OGP_IRA analysis.
#All major operations require comments.  Please refer to 4-time_series_viz
#for all related visualizations.  Any plots in this script are for diagnostic purposes.


#Load plotting theme
source("R:/AIM/Advanced Analytics/Functions/theme_clean.r")

#####Transformation Table#####
# Lambda| Transformation equation
# -2    |          1/y^2
# -.5   |          1/((sqrt)y)
# -1    |              1/y
# 0     |          lognormal(ln)
# .5    |          sqrt(y)
# 1.0   |          y
# 2.0   |          y^2

#####Create time series data.frames#####
ira_ymts_num_gift_df <- ira_yearmon %>%
  select(yearmon, num_gifts)

ira_ymts_num_ira_gifts_df <- ira_yearmon %>%
  select(yearmon, num_ira_gifts)%>%
  filter(yearmon>="2006-01-01")

#Creates a .ts format time series
ira_ymts_num_gift <- ira_ymts_num_gift_df$num_gifts
ira_ymts_num_gift <-ts(ira_ymts_num_gift, frequency=12, start=c(1997, 1))

ira_ymts_num_ira_gifts<- ira_ymts_num_ira_gifts_df$num_ira_gifts
ira_ymts_num_ira_gifts <-ts(ira_ymts_num_ira_gifts, frequency=12, start=c(2006, 1))

#####The following steps are necessary for determining the parameters of an
#ARIMA model if you are not using the auto.arima selection feature from forecast
#or the more robust and preferred x13-SEATS interface.  All steps for all methods shown below

#Check TS with plot.  Note the large gifts in 2000 and 2006 skewing the plot.
plot.ts(ira_ymts_num_gift)

plot.ts(ira_ymts_num_ira_gifts)

#Check ACF plot residuals. 3 spikes around lag 12
autoplot(acf(ira_ymts_num_gift, lag.max=20))+ theme_clean()

autoplot(acf(ira_ymts_num_ira_gifts, lag.max=20))+ theme_clean()

#Check for stationarity. If Ljung-BoxP value is >.05, non-stationary.
#P value is .612 for LB.  If KPSS is <.05, differencing is required.
#KPSS is .03
Box.test(ira_ymts_num_gift, lag=20, type="Ljung-Box")
adf.test(ira_ymts_num_gift, alternative = "stationary")
kpss.test(ira_ymts_num_gift)

#P value is .1815 for LB.  If KPSS is <.05, differencing is required.
Box.test(ira_ymts_num_ira_gifts, lag=20, type="Ljung-Box")
adf.test(ira_ymts_num_ira_gifts, alternative = "stationary")
kpss.test(ira_ymts_num_ira_gifts)

#Also can use ndiffs() from the forecast package to determine number of differences
ndiffs(ira_ymts_num_gift, test="adf") #suggests 0
ndiffs(ira_ymts_num_gift, test="kpss") #suggests 0.  Given that ADF is a unit-root test, we would go with KPSS results

#Needs to be differenced 1 time to obtain a stationary series,
#although it is our outliers producing most of the skew and seasonality.
#Shows a significant spike at lag 2 and smaller spike at lag 7.
#Note: ARIMA models must be stationary and the data may need differencing.
#Box test shows p value <.05, now stationary.
ira_ymts_dif<- diff(ira_ymts_num_gift, differences=1)
autoplot(acf(ira_ymts_dif, lag.max=20))+ theme_clean()
Box.test(ira_ymts_dif)

#Obtain lambda values for transformation if desired. Using loglik method
#due to the fact that the lambda value needs to maximize the
#profile log likelihood of a linear fitted model. Raise our original
#data to the power trans. Produces similar visual variance to log trans
ira_ymts_lambda <- BoxCox.lambda(ira_ymts_num_gift)
print(ira_ymts_lambda)

ira_ymts_trans<- ira_ymts_num_gift_df%>%
  filter(yearmon<'2016-12-01')%>%
  transmute(num_gifts=1/num_gifts)%>%
  select(num_gifts)

ira_ymts_trans<- ts(ira_ymts_trans, frequency=12, start=c(1997,1))
plot.ts(ira_ymts_trans)

#Log trans to reduce variance and compare against original plots.
#Box test shows stationarity.  Easier to back-transform than differenced data.
ira_logymts <- log(ira_ymts_num_gift)
ira_logymts <-ts(ira_logymts, frequency=12, start=c(1997, 1))
Box.test(ira_logymts)

#Check natural log TS with plot. Much less variance.  Better than
#trans because it more accurately reduces variance while retaining series
#trends.
plot.ts(ira_logymts)

#Check ACF plot residuals, 3 large auto correlations like non-transformed, but higher values
autoplot(acf(ira_logymts, lag.max=20))+ theme_clean()

ira_logymts_dif<- diff(ira_logymts, differences=2)

#Check ACF plot residuals, large auto correlations even with 2 difs.
#Should use non-transformed values
autoplot(acf(ira_logymts_dif, lag.max=20))+ theme_clean()

#Creates time series formatted data.frames
ira_ymts_num_gift_tsdf <- tsdf(ira_ymts_num_gift)
ira_ymts_trans_tsdf <- tsdf(ira_ymts_trans)
ira_ymts_ira_tsdf <- tsdf(ira_ymts_num_ira_gifts)

#Visualize decompositions
##Non-transformed 
ggsdc(ira_ymts_num_gift_tsdf, aes(x = x, y = y), method = "seas", frequency=12, start=c(1997, 1)) +
  geom_line(color="light blue")+
  theme_clean()+
  scale_y_continuous(labels = comma)+
  xlab(" ")+
  ylab(" ")+
  theme_clean()

##Power transform creates trend that is not reflective of our data. Further trans needed
#if using this method
ggsdc(ira_ymts_trans_tsdf, aes(x = x, y = y), method = "seas", frequency=12, start=c(1997, 1)) +
  geom_line(color="light blue")+
  theme_clean()+
  scale_y_continuous(labels = comma)+
  xlab(" ")+
  ylab(" ")+
  theme_clean()

##Log transformed
ggsdc(ira_ymts_ira_tsdf, aes(x = x, y = y), method = "seas", frequency=12, start=c(1997, 1)) +
  geom_line(color="light blue")+
  theme_clean()+
  scale_y_continuous(labels = comma)+
  xlab(" ")+
  ylab(" ")+
  theme_clean()

##Based on these results we would either remove outliers or use the log-transformed time series
#depending on the variable and the amount of variance it displays.  This walk-through is the minimum number
#of steps necessary to quickly visually identify time series trends.  In some cases more differencing and
#acf residual analysis is necessary.  Even if using the following X13 method these steps should be performed.

#Decomposing data into a dataframe with each component as a column. Avoids having to use
#seas function call for plotting and allows us the ability to remove the seasonal and irregular
#component.
ira_ymts_num_gift_decompose <- fortify(stats::decompose(ira_ymts_num_gift)) %>%
  rename(raw=Data, date=Index, irregular=remainder)%>%
  melt(id.vars="date")

ira_ymts_ira_decompose <- fortify(stats::decompose(ira_ymts_num_ira_gifts)) %>%
  rename(raw=Data, date=Index, irregular=remainder)%>%
  melt(id.vars="date")
#For plotting can add on gather() to make long format
ira_ymts_num_gift_decompose_long <- fortify(stats::decompose(ira_ymts_num_gift)) %>%
  rename(date=Index, raw=Data,irregular=remainder)%>%
 gather(date)

ira_ymts_ira_decompose_long <- fortify(stats::decompose(ira_ymts_num_ira_gifts)) %>%
  rename(date=Index, raw=Data,irregular=remainder)%>%
  gather(date)

#This step returns the seasonally adjusted data
ira_num_gift_seasonal <- fortify(stats::decompose(ira_ymts_num_gift)) %>%
  ungroup()%>%
  rename(date=Index, raw=Data, irregular=remainder)%>%
  mutate(adjusted=raw-seasonal) %>%
  select(date, raw, adjusted) %>%
  gather(date, raw:adjusted)

#Use this for plotting
ira_num_gift_seasonal <- fortify(stats::decompose(ira_ymts_num_gift)) %>%
  rename(date=Index, raw=Data, irregular=remainder)%>%
  melt(id.vars="date") %>%
  dcast(date~ variable, value.var="value") %>%
  group_by(date) %>%
  mutate(adjusted=raw-seasonal) %>%
  select(date, raw, adjusted) %>%
  mutate(type=as.character(ifelse(!is.na(adjusted), "num", "num"))) %>%
  melt(id.vars=c("date","type"))

ira_seasonal_ira <- fortify(stats::decompose(ira_ymts_num_ira_gifts)) %>%
  ungroup()%>%
  rename(date=Index, raw=Data, irregular=remainder)%>%
  mutate(adjusted=raw-seasonal) %>%
  select(date, raw, adjusted) %>%
  gather(date, data, raw:adjusted)

#Use this for plotting
ira_seasonal_ira <- fortify(stats::decompose(ira_ymts_num_ira_gifts)) %>%
  rename(date=Index, raw=Data, irregular=remainder)%>%
  melt(id.vars="date") %>%
  dcast(date~ variable, value.var="value") %>%
  group_by(date) %>%
  mutate(adjusted=raw-seasonal) %>%
  select(date, raw, adjusted) %>%
  mutate(type=as.character(ifelse(!is.na(adjusted), "ira", "ira"))) %>%
  melt(id.vars=c("date","type"))

#Combine both datasets into one data frame to plot on the same plot
#By selecting source as the color in aes, and setting y to value you can easily plot
#both time series using geom_line

ira_adjusted<- ira_num_gift_seasonal%>%
  bind_rows(ira_seasonal_ira)
  
ira_adjusted$type<- gsub("num", "Non-IRA", ira_adjusted$type)
ira_adjusted$type<- gsub("ira", "IRA", ira_adjusted$type)

######Seasonal adjustment using X-13Arima-SEATS.  Works optimally in most
#circumstances.  DO NOT START WITH A POWER-TRANSFORMED TS.  The package automatically
#runs an AIC test and picks the transformation.

#Start with our differenced total
ira_num_gift_x13 <- seas(ira_ymts_num_gift)

ira_x13_num_ira_gifts <- seas(ira_ymts_num_ira_gifts)

#Launches GUI to inspect and tweak x13 values
inspect(ira_num_gift_x13)

inspect(ira_x13_num_ira_gifts)

#Final time series data of the adjusted model
final_num_gift_x13 <- final(ira_num_gift_x13)

final_num_ira_gifts_x13 <- final(ira_x13_num_ira_gifts)

#Create data.frame and format date
final_num_gift_x13df <- tsdf(final_num_gift_x13)
final_num_gift_x13df$date <- as.Date(yearmon(final_num_gift_x13df$x))

final_num_ira_gifts_x13df <- tsdf(final_num_ira_gifts_x13)
final_num_ira_gifts_x13df$date <- as.Date(yearmon(final_num_ira_gifts_x13df$x))

######Obtaining forecasts from the modeled data#####
#If using the seasonal package this is how the forecast is extracted from the model
ira_num_gift_x13_forecast<- series(ira_num_gift_x13, "forecast.forecasts")

ira_x13_forecast_num_ira_gifts<- series(ira_x13_num_ira_gifts, "forecast.forecasts")

#If using the auto.Arima method from the forecast package this is how the model is extracted
ira_auto_arima_num_gift<- auto.arima(ira_ymts_num_gift)
ira_auto_forecast_num_gift<- forecast(ira_auto_arima_num_gift, h=24)

ira_auto_arima_num_ira_gifts- auto.arima(ira_ymts_num_ira_gifts)
ira_auto_forecast_num_ira_gifts<- forecast(ira_auto_arima_num_ira_gifts, h=24)

#For plotting and easy printing, convert to tsdf from ggseas package
ira_x13_fore_ts_num_gift<- ts(ira_x13_forecast_num_gift, start=2017, frequency = 12)
ira_x13_fore_df_num_gift<-tsdf(ira_x13_fore_ts_num_gift)

ira_x13_fore_ts_num_ira_gifts<- ts(ira_x13_forecast_num_ira_gifts, start=2017, frequency = 12)
ira_x13_fore_df_num_ira_gifts<-tsdf(ira_x13_fore_ts_num_ira_gifts)

#For forecast package objects, use fortify from ggfortify in a single call
ira_auto_forecast_df_num_gift<- fortify(forecast::forecast(ira_auto_arima_num_gift, h=24))

ira_auto_forecast_df_num_ira_gifts<- fortify(forecast::forecast(ira_auto_arima_num_ira_gifts, h=24))

#or for control over confidence intervals
ira_auto_arima_num_gift<- auto.arima(ira_ymts_num_gift)
ira_auto_forecast_num_gift <- forecast.Arima(ira_auto_arima_num_gift, level = c(85), h = 60)

ira_auto_arima_num_ira_gifts<- auto.arima(ira_ymts_num_ira_gifts)
ira_auto_forecast_num_ira_gifts <- forecast.Arima(ira_auto_arima_num_ira_gifts, level = c(85), h = 60)
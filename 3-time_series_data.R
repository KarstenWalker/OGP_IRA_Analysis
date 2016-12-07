library(ggfortify)
library(scales)
library(tidyr)
library(tseries)
library(seasonal)
library(ggseas)
library(forecast)
library(zoo)
library(xts)
library(shiny)

#####About#####

#This file is for all time series analysis related to the OGP_IRA analysis.
#All major operations require comments.  Please refer to 4-time_series_viz
#for all related visualizations.  Any plots in this script are for diagnostic purposes.

#Plotting theme
theme_kw <- theme_bw() +
  theme(axis.title=element_text(face="bold", size= 8),
        plot.title=element_text(face = "bold", size= 10),
        strip.text.x = element_text(size= 7),
        strip.text.y = element_text(size= 7),
        panel.border=element_blank(),
        axis.text = element_text(size = 7),
        plot.background=element_rect(size= 1, color = "grey37"),
        strip.background =element_blank(),
        legend.title= element_text(face="bold", size= 8),
        legend.text= element_text(face="bold", size= 7),
        legend.position="bottom",
        legend.key=element_blank()
  )

#####Transformation Table#####
# Lambda| Transformation equation
# -2    |          1/y^2
# -.5   |          1/((sqrt)y)
# -1    |              1/y
# 0     |          lognormal(ln)
# .5    |          sqrt(y)
# 1.0   |          y
# 2.0   |          y^2

#Create time series data.frame for total giving
ira_ymts_df <- ira_yearmon %>%
  select(yearmon, total_giving)

#Creates a .ts format time series
ira_ymts <- ira_ymts_df$total_giving
ira_ymts <-ts(ira_ymts, frequency=12, start=c(1997, 1))

#####The following steps are necessary for determining the parameters of an
#ARIMA model if you are not using the auto.arima selection feature from forecast
#or the more robust and preferred x13-SEATS interface.  All steps for all methods shown below

#Check TS with plot.  Note the large gifts in 2000 and 2006 skewing the plot.
plot.ts(ira_ymts)

#Check ACF plot residuals. 3 spikes around lag 12
autoplot(acf(ira_ymts, lag.max=20))+ theme_kw

#Check for stationarity. If Ljung-BoxP value is >.05, non-stationary.
#P value is .757 for LB.  If KPSS is <.05, differencing is required.
Box.test(ira_ymts, lag=20, type="Ljung-Box")
adf.test(ira_ymts, alternative = "stationary")
kpss.test(ira_ymts)

#Also can use ndiffs() from the forecast package to determine number of differences
ndiffs(ira_ymts, test="adf") #suggests 0
ndiffs(ira_ymts, test="kpss") #suggests 1.  Given that ADF is a unit-root test, we would go with KPSS results

#Needs to be differenced 1 time to obtain a stationary series,
#although it is our outliers producing most of the skew and seasonality.
#Shows a significant spike at lag 2 and smaller spike at lag 7.
#Note: ARIMA models must be stationary and the data may need differencing.
#Box test shows p value <.05, now stationary.
ira_ymts_dif<- diff(ira_ymts, differences=1)
autoplot(acf(ira_ymts_dif, lag.max=20))+ theme_kw
Box.test(ira_ymts_dif)

#Obtain lambda values for transformation if desired. Using loglik method
#due to the fact that the lambda value needs to maximize the
#profile log likelihood of a linear fitted model. Raise our original
#data to the power trans. Produces similar visual variance to log trans
ira_ymts_lambda <- BoxCox.lambda(ira_ymts)
print(ira_ymts_lambda)

ira_ymts_trans<- ira_ymts_df%>%
  transmute(total_giving=total_giving^ira_ymts_lambda)%>%
  select(total_giving)

ira_ymts_trans<- ts(ira_ymts_trans, frequency=12, start=c(1997,1))
plot.ts(ira_ymts_trans)

#Log trans to reduce variance and compare against original plots.
#Box test shows stationarity.  Easier to back-transform than differenced data.
ira_logymts <- log(ira_ymts)
ira_logymts <-ts(ira_logymts, frequency=12, start=c(1997, 1))
Box.test(ira_logymts)

#Check natural log TS with plot. Much less variance.  Better than
#trans because it more accurately reduces variance while retaining series
#trends.
plot.ts(ira_logymts)

#Check ACF plot residuals, 3 large auto correlations like non-transformed, but higher values
autoplot(acf(ira_logymts, lag.max=20))+ theme_kw

ira_logymts_dif<- diff(ira_logymts, differences=2)

#Check ACF plot residuals, large auto correlations even with 2 difs.
#Should use non-transformed values
autoplot(acf(ira_logymts_dif, lag.max=20))+ theme_kw

#Creates time series formatted data.frames
ira_ymts_tsdf <- tsdf(ira_ymts)
ira_ymts_trans_tsdf <- tsdf(ira_ymts_trans)
ira_ymts_log_tsdf <- tsdf(ira_logymts)

#Visualize decompositions
##Non-transformed 
ggsdc(ira_ymts_tsdf, aes(x = x, y = y), method = "seas", frequency=12, start=c(1997, 1)) +
  geom_line(color="light blue")+
  theme_kw+
  scale_y_continuous(labels = comma)+
  xlab(" ")+
  ylab(" ")+
  theme_kw

##Power transform creates trend that is not reflective of our data. Further trans needed
#if using this method
ggsdc(ira_ymts_trans_tsdf, aes(x = x, y = y), method = "seas", frequency=12, start=c(1997, 1)) +
  geom_line(color="light blue")+
  theme_kw+
  scale_y_continuous(labels = comma)+
  xlab(" ")+
  ylab(" ")+
  theme_kw

##Log transformed
ggsdc(ira_ymts_log_tsdf, aes(x = x, y = y), method = "seas", frequency=12, start=c(1997, 1)) +
  geom_line(color="light blue")+
  theme_kw+
  scale_y_continuous(labels = comma)+
  xlab(" ")+
  ylab(" ")+
  theme_kw

##Based on these results we would either remove outliers or use the log-transformed time series
#depending on the variable and the amount of variance it displays.  This walk-through is the minimum number
#of steps necessary to quickly visually identify time series trends.  In some cases more differencing and
#acf residual analysis is necessary.  Even if using the following X13 method these steps should be performed.

#Decomposing data into a dataframe with each component as a column. Avoids having to use
#seas function call for plotting and allows us the ability to remove the seasonal and irregular
#component.
ira_ymts_decompose <- fortify(stats::decompose(ira_ymts)) %>%
  rename(date=Index, irregular=remainder)%>%
  melt(id.vars="date")

#For plotting can add on gather() to make long format
ira_ymts_decompose_long <- fortify(stats::decompose(ira_ymts)) %>%
  rename(date=Index, raw=Data,irregular=remainder)%>%
 gather(date)

#This step returns the seasonally adjusted data
ira_seasonal <- fortify(stats::decompose(ira_ymts)) %>%
  ungroup()%>%
  rename(date=Index, raw=Data, irregular=remainder)%>%
  mutate(adjusted=raw-seasonal) %>%
  select(date, raw, adjusted) %>%
  gather(date, data, raw:adjusted)

ira_seasonal <- fortify(stats::decompose(ira_ymts)) %>%
  rename(date=Index, raw=Data, irregular=remainder)%>%
  melt(id.vars="date") %>%
  dcast(date~ variable, value.var="value") %>%
  group_by(date) %>%
  mutate(adjusted=raw-seasonal) %>%
  select(date, raw, adjusted) %>%
  melt(id.vars="date")

######Seasonal adjustment using X-13Arima-SEATS.  Works optimally in most
#circumstances.  DO NOT START WITH A POWER-TRANSFORMED TS.  The package automatically
#runs an AIC test and picks the transformation.

#Start with our differenced log-transformed total
ira_x13 <- seas(ira_ymts)

#Launches GUI to inspect and tweak x13 values
inspect(ira_x13)

#Final time series data of the adjusted model
final_ira_x13 <- final(ira_x13)

#Create data.frame and format date
final_ira_x13df <- tsdf(final_ira_x13)
final_ira_x13df$date <- as.Date(yearmon(final_ira_x13df$x))

######Obtaining forecasts from the modeled data#####
#If using the seasonal package this is how the forecast is extracted from the model
ira_x13_forecast<- series(ira_x13, "forecast.forecasts")

#If using the auto.Arima method from the forecast package this is how the model is extracted
ira_auto_arima<- auto.arima(ira_ymts)
ira_auto_forecast<- forecast(ira_auto_arima, h=24)

#For plotting and easy printing, convert to tsdf from ggseas package
ira_x13_fore_ts<- ts(ira_x13_forecast, start=2017, frequency = 12)
ira_x13_fore_df<-tsdf(ira_x13_fore_ts)

#Fore forecast package objects, use fortify from ggfortify in a single call
ira_auto_forecast_df<- fortify(forecast::forecast(ira_auto_arima, h=24))

#or for control over confidence intervals
ira_auto_arima<- auto.arima(ira_ymts)
ira_auto_forecast <- forecast.Arima(ira_auto_arima, level = c(85), h = 60)
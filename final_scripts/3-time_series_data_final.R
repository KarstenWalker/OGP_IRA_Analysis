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

#Loy
ira_ymts_num_non_ira_gifts_df <- ira_yearmon %>%
  select(yearmon, num_non_ira_gifts)

#Creates a .ts format time series
ira_ymts_num_gift <- ira_ymts_num_gift_df$num_gifts
ira_ymts_num_gift <-ts(ira_ymts_num_gift, frequency=12, start=c(1995, 1))

ira_ymts_num_ira_gifts<- ira_ymts_num_ira_gifts_df$num_ira_gifts
ira_ymts_num_ira_gifts <-ts(ira_ymts_num_ira_gifts, frequency=12, start=c(2006, 1))
#Loy
ira_ymts_num_non_ira_gifts<- ira_ymts_num_non_ira_gifts_df$num_non_ira_gifts
ira_ymts_num_non_ira_gifts <-ts(ira_ymts_num_non_ira_gifts, frequency=12, start=c(1995, 1))

#####The following steps are necessary for determining the parameters of an
#ARIMA model if you are not using the auto.arima selection feature from forecast
#or the more robust and preferred x13-SEATS interface.  All steps for all methods shown below



#This step returns the seasonally adjusted data
ira_num_gift_seasonal <- fortify(stats::decompose(ira_ymts_num_gift)) %>%
  ungroup()%>%
  rename(date=Index, raw=Data, irregular=remainder)%>%
  mutate(adjusted=raw-seasonal) %>%
  select(date, raw, adjusted) %>%
  gather(date, raw:adjusted)

#FOR PLOTTING
ira_num_gift_seasonal <- fortify(stats::decompose(ira_ymts_num_gift)) %>%
  rename(date=Index, raw=Data, irregular=remainder)%>%
  melt(id.vars="date") %>%
  dcast(date~ variable, value.var="value") %>%
  group_by(date) %>%
  mutate(adjusted=raw-seasonal) %>%
  select(date, raw, trend, adjusted) %>%
  mutate(type=as.character(ifelse(!is.na(adjusted), "All", "All"))) %>%
  melt(id.vars=c("date","type"))

ira_seasonal_ira <- fortify(stats::decompose(ira_ymts_num_ira_gifts)) %>%
  ungroup()%>%
  rename(date=Index, raw=Data, irregular=remainder)%>%
  mutate(adjusted=raw-seasonal) %>%
  select(date, raw, adjusted) %>%
  gather(date, data, raw:adjusted)

#FOR PLOTTING
ira_seasonal_ira <- fortify(stats::decompose(ira_ymts_num_ira_gifts)) %>%
  rename(date=Index, raw=Data, irregular=remainder)%>%
  melt(id.vars="date") %>%
  dcast(date~ variable, value.var="value") %>%
  group_by(date) %>%
  mutate(adjusted=raw-seasonal) %>%
  select(date, raw, trend, adjusted) %>%
  mutate(type=as.character(ifelse(!is.na(adjusted), "ira", "ira"))) %>%
  melt(id.vars=c("date","type"))

#FOR PLOTTING Loy
ira_seasonal_non_ira <- fortify(stats::decompose(ira_ymts_num_non_ira_gifts)) %>%
  ungroup()%>%
  rename(date=Index, raw=Data, irregular=remainder)%>%
  mutate(adjusted=raw-seasonal) %>%
  select(date, raw, adjusted) %>%
  gather(date, data, raw:adjusted)

ira_seasonal_non_ira <- fortify(stats::decompose(ira_ymts_num_non_ira_gifts)) %>%
  rename(date=Index, raw=Data, irregular=remainder)%>%
  melt(id.vars="date") %>%
  dcast(date~ variable, value.var="value") %>%
  group_by(date) %>%
  mutate(adjusted=raw-seasonal) %>%
  select(date, raw, trend, adjusted) %>%
  mutate(type=as.character(ifelse(!is.na(adjusted), "non_ira", "non_ira"))) %>%
  melt(id.vars=c("date","type"))

#Combine both datasets into one data frame to plot on the same plot
#By selecting source as the color in aes, and setting y to value you can easily plot
#both time series using geom_line

ira_adjusted<- ira_num_gift_seasonal%>%
  bind_rows(ira_seasonal_ira)%>%
  bind_rows(ira_seasonal_non_ira)
  
ira_adjusted$type<- gsub("All", "All", ira_adjusted$type)
ira_adjusted$type<- gsub("ira", "IRA", ira_adjusted$type)
ira_adjusted$type<- gsub("non_ira", "Non-IRA", ira_adjusted$type)
ira_adjusted <- ira_adjusted %>% filter(date<=Sys.Date())


#####Plots for mean amounts#####
ira_ymts_mean_df <- ira_yearmon %>%
  select(yearmon, mean_amt)

ira_ymts_ira_mean_df <- ira_yearmon %>%
  select(yearmon, mean_ira_amt)%>%
  filter(yearmon>="2006-01-01")

ira_ymts_non_ira_mean_df <- ira_yearmon %>%
  select(yearmon, mean_non_ira_amt)

ira_ymts_mean <- ira_ymts_mean_df$mean_amt
ira_ymts_mean <-ts(ira_ymts_mean, frequency=12, start=c(1995, 1))

ira_ymts_ira_mean <- ira_ymts_ira_mean_df$mean_ira_amt
ira_ymts_ira_mean <-ts(ira_ymts_ira_mean, frequency=12, start=c(2006, 1))

ira_ymts_non_ira_mean <- ira_ymts_non_ira_mean_df$mean_non_ira_amt
ira_ymts_non_ira_mean <-ts(ira_ymts_non_ira_mean, frequency=12, start=c(1995, 1))

ira_mean_seasonal <- fortify(stats::decompose(ira_ymts_mean)) %>%
  rename(date=Index, raw=Data, irregular=remainder)%>%
  melt(id.vars="date") %>%
  dcast(date~ variable, value.var="value") %>%
  group_by(date) %>%
  mutate(adjusted=raw-seasonal) %>%
  select(date, raw,trend, adjusted) %>%
  mutate(type=as.character(ifelse(!is.na(adjusted), "All", "All"))) %>%
  melt(id.vars=c("date","type"))

ira_ira_mean_seasonal <- fortify(stats::decompose(ira_ymts_ira_mean)) %>%
  rename(date=Index, raw=Data, irregular=remainder)%>%
  melt(id.vars="date") %>%
  dcast(date~ variable, value.var="value") %>%
  group_by(date) %>%
  mutate(adjusted=raw-seasonal) %>%
  select(date, raw, trend, adjusted) %>%
  mutate(type=as.character(ifelse(!is.na(adjusted), "IRA", "IRA"))) %>%
  melt(id.vars=c("date","type"))


ira_non_ira_mean_seasonal <- fortify(stats::decompose(ira_ymts_non_ira_mean)) %>%
  rename(date=Index, raw=Data, irregular=remainder)%>%
  melt(id.vars="date") %>%
  dcast(date~ variable, value.var="value") %>%
  group_by(date) %>%
  mutate(adjusted=raw-seasonal) %>%
  select(date, raw, trend, adjusted) %>%
  mutate(type=as.character(ifelse(!is.na(adjusted), "Non-IRA", "Non-IRA"))) %>%
  melt(id.vars=c("date","type"))

ira_adjusted_mean<- ira_mean_seasonal%>%
  bind_rows(ira_ira_mean_seasonal)%>%
  bind_rows(ira_non_ira_mean_seasonal)
ira_adjusted_mean<- ira_adjusted_mean%>%filter(date<=Sys.Date())
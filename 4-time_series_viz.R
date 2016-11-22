library(ggseas)
library(ggplot2)
library(scales)
library(seasonal)
library(forecast)

#Decomposition using ggseas
ggsdc(ira_ymts_tsdf, aes(x = x, y = y), method = "seas", frequency=12, start=c(1997, 1)) +
  geom_line(color="light blue")+
  theme_kw+
  scale_y_continuous(labels = comma)+
  xlab(" ")+
  ylab(" ")+
  theme_kw

#Seasonally transformed data using ggplot. Requires data.frame with both raw
#and adjusted values.  Can use facets to separate or include on one plot overlaid.
ggplot()+
  geom_line(data=ira_seasonal, aes(x=date, y=value, color=variable))+
  ggtitle("Seasonally Transformed Giving Total")+
  scale_y_continuous(labels = comma)+
  scale_colour_manual(values = c("light green", "light blue"))+
  theme_kw+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.background=element_blank(),
        legend.title=element_blank())

#Seasonal transformation and plotting in 1 step using ggseas. Requires the
#non-adjusted time series
ira_ymts_fort<-fortify(ira_ymts)
ira_ymts_fort$x<-as.yearmon(ira_ymts_fort$x)
ggplot(data=ira_ymts_fort, aes(x=Index, y=Data))+
  geom_line(color="light green")+
  stat_seas(color="light blue", start = c(1997, 1), frequency=12)+
  scale_y_continuous(labels = comma)+
  theme_kw+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.background=element_blank(),
        legend.title=element_blank())


#If using the seasonal package this is how the forecast is extracted from the model
ira_x13_forecast<- series(ira_x13, "forecast.forecasts")
m3 <-series(m, "forecast.forecasts")
m4 <- ts(m3, start=2016, frequency = 12)
foredf<- tsdf(m4)
foredf$date <- as.Date(yearmon(foredf$x))

#If using the auto.Arima method from the forecast package this is how the model is extracted
ira_auto_arima<- auto.arima(ira_ymts)

plot(forecast(ira_auto_arima, h=120))


ggplot()+
  geom_line(data=final_ira_x13df, aes(x=x, y=y, colour="Seasonally Adjusted"))+
  geom_line(data=ira_x13_fore_df, aes(x=x, y=forecast, colour="Forecast"))+
  geom_ribbon(data=ira_x13_fore_df, aes(x=x, ymax=upperci, ymin=lowerci), fill="grey70", alpha=.2)+
  theme_kw


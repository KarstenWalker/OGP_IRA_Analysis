library(ggseas)
library(ggplot2)
library(scales)
library(seasonal)
library(forecast)

#Load plotting theme
source("R:/AIM/Advanced Analytics/Functions/theme_clean.r")

#Load legend move function
source("R:/AIM/Advanced Analytics/Functions/legend_move.r")

#Both seasonally adjusted metric on one plot
ggplot()+
  geom_line(data=(ira_adjusted%>%
                    filter(variable=="raw")),
            aes(x=date, y=value, colour=source), size=1)+
  geom_vline(aes(xintercept =as.numeric(as.Date("2006-01-01"))),color="blue", size=1)+
  ylim(0,300)+
  scale_x_date()+
  theme_clean()


#Decomposition using ggseas
ggsdc(ira_ymts_num_tsdf, aes(x = x, y = y), method = "seas", frequency=12, start=c(1997, 1)) +
  geom_line(color="light blue")+
  scale_y_continuous(labels = comma)+
  xlab(" ")+
  ylab(" ")+
  theme_clean()

ggsdc(ira_ymts_ira_tsdf, aes(x = x, y = y), method = "seas", frequency=12, start=c(1997, 1)) +
  geom_line(color="light blue")+
  scale_y_continuous(labels = comma)+
  xlab(" ")+
  ylab(" ")+
  theme_clean()


#Seasonally transformed data using ggplot. Requires data.frame with both raw
#and adjusted values.  Can use facets to separate or include on one plot overlaid.
ggplot()+
  geom_line(data=ira_num_seasonal, aes(x=date, y=value, color=variable))+
  ggtitle("Seasonally Transformed Number of Gifts")+
  scale_y_continuous(labels = comma)+
  scale_colour_manual(values = c("light green", "light blue"))+
  theme_clean()+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.background=element_blank(),
        legend.title=element_blank())

ggplot()+
  geom_line(data=ira_seasonal_ira, aes(x=date, y=value, color=variable))+
  ggtitle("Seasonally Transformed Number of Gifts")+
  scale_y_continuous(labels = comma)+
  scale_colour_manual(values = c("light green", "light blue"))+
  theme_clean()+
  ylim(0,150)+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.background=element_blank(),
        legend.title=element_blank())

#Seasonal transformation and plotting in 1 step using ggseas. Requires the
#non-adjusted time series
ira_ymts_fort_num<-fortify(ira_ymts_num)
ira_ymts_fort_num$Index<-as.yearmon(ira_ymts_fort_num$Index)

ggplot(data=ira_ymts_fort, aes(x=Index, y=Data))+
  geom_line(color="light green")+
  stat_seas(color="light blue", start = c(1997, 1), frequency=12)+
  scale_y_continuous(labels = comma)+
  theme_clean()+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.background=element_blank(),
        legend.title=element_blank())

ira_ymts_fort_ira<-fortify(ira_ymts_ira)
ira_ymts_fort$Index<-as.yearmon(ira_ymts_fort$Index)

ggplot(data=ira_ymts_fort_ira, aes(x=Index, y=Data))+
  geom_line(color="light green")+
  stat_seas(color="light blue", start = c(1997, 1), frequency=12)+
  scale_y_continuous(labels = comma)+
  theme_clean()+
  ylim(0,150)+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.background=element_blank(),
        legend.title=element_blank())

#If using the seasonal package this is how the forecast is extracted from the model
ira_x13_forecast<- series(ira_x13, "forecast.forecasts")
ira_x13_
foredf<- tsdf(m4)
foredf$date <- as.Date(yearmon(foredf$x))

#If using the auto.Arima method from the forecast package can use the autoplot
#function, which is just a ggplot function
autoplot(ira_auto_forecast)+
  theme_kw +ggtitle("")+scale_y_log10()


ggplot()+
  geom_line(data=final_ira_x13df, aes(x=x, y=y, colour="Seasonally Adjusted"))+
  geom_line(data=ira_x13_fore_df, aes(x=x, y=forecast, colour="Forecast"))+
  geom_ribbon(data=ira_x13_fore_df, aes(x=x, ymax=upperci, ymin=lowerci), fill="grey70", alpha=.2)+
  theme_kw


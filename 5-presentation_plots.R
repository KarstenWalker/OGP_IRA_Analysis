library(dplyr)
library(ggplot2)
library(randomcoloR)

#####About#####
#This file is for the first round of presentation plots.
#Please note any changes in axis', scales, or other transformations in the plot comments

#Load plotting theme
source("R:/AIM/Advanced Analytics/Functions/theme_clean.r")

#Load legend move function
source("R:/AIM/Advanced Analytics/Functions/legend_move.r")

#Colorblind palatte
pal_blind_type <- c("#E69F00", "#56B4E9", "#009E73")


#Map to type
types<-unlist(ira_adjusted%>%
                group_by(type)%>%
                summarize()%>%
                select(type))

names(pal_blind_type)<-types

#Avg time between gifts by transaction age (current or age at death) by IRA vs Non-IRA
#Uncomment last line to move legend onto the plot.  Sometimes works better for slides projected onto a big screen.

#Plot no highlights
ggplot(ira_adj%>%
         ungroup()%>%
         filter(norm_outlier_score<.20 |decile!="Top"| decile !="10" &
                  age>=70.5 &num_ira_gifts>1)%>%
         select(ira_law,age,time_between, adj_amt)%>%
         mutate(age=trunc(age))%>%
         group_by(age, ira_law)%>%
         summarize(time_between=mean(time_between),
                   avg_amt=mean(adj_amt))%>%
         filter(time_between>0)
       )+
  geom_point(aes(x=age,y=time_between,color=as.factor(ira_law), size=avg_amt))+
  xlim(70.5,95)+
  xlab("Age At Time of Gift")+
  ylim(0,400)+
  ylab("Frequency of Giving (days)")+
  scale_color_discrete(name="IRA Law", labels=c("Before", "After"))+
  scale_size_continuous(range=c(0,20),name="Average Gift Amount",guide = guide_legend(override.aes = list(colour = "#F8766D")))+
  geom_smooth (aes(x=age,y=time_between,color=as.factor(ira_law)), se=FALSE)+
  ggtitle("Donors Give More, But Less Frequently After Law")+
  theme_clean()+
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.box="horizontal",
        legend.key=element_rect(fill="white"),
        axis.text = element_text(size = 12),
        axis.title=element_text(face="bold", size= 16),
        plot.title=element_text(face = "bold", size= 24),
        legend.background =element_rect(fill="transparent", colour=NA))

#Plot highlight for time between
ggplot(ira_adj%>%
         ungroup()%>%
         filter(norm_outlier_score<.20 |decile!="Top"| decile !="10" &
                  age>=70.5 &num_ira_gifts>1)%>%
         select(ira_law,age,time_between, adj_amt)%>%
         mutate(age=trunc(age))%>%
         group_by(age, ira_law)%>%
         summarize(time_between=mean(time_between),
                   avg_amt=mean(adj_amt))%>%
         filter(time_between>0)
       )+
  geom_point(aes(x=age,y=time_between,color=as.factor(ira_law), size=avg_amt))+
  geom_rect(data=(ira_adj%>%
              ungroup()%>%
              filter(norm_outlier_score<.20 |decile!="Top"| decile !="10")
            %>%
              select(ira_law,age,time_between, adj_amt)%>%
              filter(age>=70.5)%>%
              mutate(age=trunc(age))%>%
              group_by(age, ira_law)%>%
              summarize(time_between=mean(time_between))%>%
              filter(age==71)), aes(xmin=age-.5, xmax=age+.5, ymin=min(time_between)-10, ymax=max(time_between)+10), fill="light blue",
            alpha=.5)+
  xlim(70.5,95)+
  xlab("Age At Time of Gift")+
  ylim(0,400)+
  ylab("Frequency of Giving (days)")+
  scale_color_discrete(name="IRA Law", labels=c("Before", "After"))+
  scale_size_continuous(range=c(0,20),name="Average Gift Amount",guide = guide_legend(override.aes = list(colour = "#F8766D")))+
  geom_smooth (aes(x=age,y=time_between,color=as.factor(ira_law)), se=FALSE)+
  ggtitle("Donors Give More, But Less Frequently After Law")+
  theme_clean()+
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.box="horizontal",
        legend.key=element_rect(fill="white"),
        axis.text = element_text(size = 12),
        axis.title=element_text(face="bold", size= 16),
        plot.title=element_text(face = "bold", size= 24),
        legend.background =element_rect(fill="transparent", colour=NA))

#Plot highlight for amount
ggplot(ira_adj%>%
         ungroup()%>%
         filter(norm_outlier_score<.20 |decile!="Top"| decile !="10" &
                  age>=70.5 &num_ira_gifts>1)%>%
         select(ira_law,age,time_between, adj_amt)%>%
         mutate(age=trunc(age))%>%
         group_by(age, ira_law)%>%
         summarize(time_between=mean(time_between),
                   avg_amt=mean(adj_amt))%>%
         filter(time_between>0)
       )+
  geom_point(aes(x=age,y=time_between,color=as.factor(ira_law), size=avg_amt))+
  geom_rect(data=(ira_adj%>%
                    ungroup()%>%
                    filter(norm_outlier_score<.20 |decile!="Top"| decile !="10")
                  %>%
                    select(ira_law,age,time_between, adj_amt)%>%
                    filter(age>=70.5)%>%
                    mutate(age=trunc(age))%>%
                    group_by(age, ira_law)%>%
                    summarize(time_between=mean(time_between))%>%
                    filter(age==75)), aes(xmin=age-.5, xmax=age+.5, ymin=min(time_between)-10, ymax=max(time_between)+10), fill="light blue",
            alpha=.5)+
  xlim(70.5,95)+
  xlab("Age At Time of Gift")+
  ylim(0,400)+
  ylab("Frequency of Giving (days)")+
  scale_color_discrete(name="IRA Law", labels=c("Before", "After"))+
  scale_size_continuous(range=c(0,20),name="Average Gift Amount",guide = guide_legend(override.aes = list(colour = "#F8766D")))+
  geom_smooth (aes(x=age,y=time_between,color=as.factor(ira_law)), se=FALSE)+
  ggtitle("Donors Give More, But Less Frequently After Law")+
  theme_clean()+
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.box="horizontal",
        legend.key=element_rect(fill="white"),
        axis.text = element_text(size = 12),
        axis.title=element_text(face="bold", size= 16),
        plot.title=element_text(face = "bold", size= 24),
        legend.background =element_rect(fill="transparent", colour=NA))

#Raw IRA gift counts
ggplot()+
  geom_line(data=(ira_adjusted%>%
                    filter(variable=="raw")),
            aes(x=date, y=value, colour=type), size=1)+
  geom_vline(aes(xintercept =as.numeric(as.Date("2006-01-01"))),color="blue", size=1)+
  ggtitle("Trends in IRA Gifts \nare not obvious at first glance")+
  scale_x_date()+
  scale_colour_discrete(name="Gift Type")+
  xlab("")+
  ylab("Number of Gifts")+
  theme_clean()+
  theme(axis.text = element_text(size = 10),
        axis.title=element_text(face="bold", size= 16),
        plot.title=element_text(face = "bold", size= 24),
        legend.key=element_rect(fill="white"),
        legend.background =element_rect(fill="transparent", colour=NA))


#Add smooth lines
ggplot()+
  geom_line(data=(ira_adjusted%>%
                    filter(variable=="raw")),
            aes(x=date, y=value, colour=type), size=1,alpha=0.2)+
  geom_vline(aes(xintercept =as.numeric(as.Date("2006-01-01"))),color="blue", size=1)+
  ggtitle("Examining the trends hints at \na need for a deeper analysis")+
  scale_x_date()+
  scale_colour_discrete(name="Gift Type")+
  xlab("")+
  ylab("Number of Gifts")+
  theme_clean()+
  theme(axis.text = element_text(size = 10),
        axis.title=element_text(face="bold", size= 16),
        plot.title=element_text(face = "bold", size= 24),
        legend.key=element_rect(fill="white"),
        legend.background =element_rect(fill="transparent", colour=NA))+
  geom_smooth(
    data=(ira_adjusted%>%filter(date<"2006-01-01")%>%
            filter(variable=="raw")),
    aes(x=date, y=value, colour=type), size=1,se=FALSE)+
  geom_smooth(
    data=(ira_adjusted%>%filter(date>="2006-01-01")%>%
            filter(variable=="raw")),
    aes(x=date, y=value, colour=type), size=1,se=FALSE)


#Same plot, but using trend component only 
ggplot()+
  geom_line(data=(ira_adjusted%>%
                    filter(variable=="trend")),
            aes(x=date, y=value, colour=type), size=1,alpha=0.2)+
  geom_vline(aes(xintercept =as.numeric(as.Date("2006-01-01"))),color="blue", size=1)+
  ggtitle("Upon removing seasonality, IRA gifts do not appear \nas supplementary vehicles for the IRA population")+
  scale_x_date()+
  scale_colour_discrete(name="Gift Type")+
  xlab("")+
  ylab("Number of Gifts")+
  theme_clean()+
  theme(axis.text = element_text(size = 10),
        axis.title=element_text(face="bold", size= 16),
        plot.title=element_text(face = "bold", size= 24),
        legend.key=element_rect(fill="white"),
        legend.background =element_rect(fill="transparent", colour=NA))+
  geom_smooth(
    data=(ira_adjusted%>%filter(date<"2006-01-01")%>%
            filter(variable=="trend")),
    aes(x=date, y=value, colour=type), size=1,se=FALSE)+
  geom_smooth(
    data=(ira_adjusted%>%filter(date>="2006-01-01")%>%
            filter(variable=="trend")),
    aes(x=date, y=value, colour=type), size=1,se=FALSE)


#Same plot, but using trend component only and campaign overlay 
ggplot()+
  geom_line(data=(ira_adjusted%>%
                    filter(variable=="trend")),
            aes(x=date, y=value, colour=type), size=1,alpha=0.2)+
  geom_vline(aes(xintercept =as.numeric(as.Date("2006-01-01"))),color="blue", size=1)+
  geom_rect(data=campaigndates[2:3,], 
            mapping=aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), color='grey', alpha=0.2)+
  ggtitle("Upon removing seasonality, IRA gifts do not appear \nas supplementary vehicles for the IRA population")+
  scale_x_date()+
  scale_colour_discrete(name="Gift Type")+
  xlab("")+
  ylab("Number of Gifts")+
  theme_clean()+
  theme(axis.text = element_text(size = 10),
        axis.title=element_text(face="bold", size= 16),
        plot.title=element_text(face = "bold", size= 24),
        legend.key=element_rect(fill="white"),
        legend.background =element_rect(fill="transparent", colour=NA))+
  geom_smooth(
    data=(ira_adjusted%>%filter(date<"2006-01-01")%>%
            filter(variable=="trend")),
    aes(x=date, y=value, colour=type), size=1,se=FALSE)+
  geom_smooth(
    data=(ira_adjusted%>%filter(date>="2006-01-01")%>%
            filter(variable=="trend")),
    aes(x=date, y=value, colour=type), size=1,se=FALSE)





#Average gift amount and year for IRA, non-IRA and All gifts.  No Trend lines.
  ggplot()+
  geom_line(data=(ira_adjusted_mean%>%
                    filter(variable=="raw")),
            aes(x=date, y=value, colour=type), size=1)+
  geom_vline(aes(xintercept =as.numeric(as.Date("2006-01-01"))),color="blue", size=1)+
  ggtitle("Trends in Average Gift Amounts \nare not obvious at first glance")+
  scale_y_continuous(labels=comma)+
  scale_x_date()+
  xlab("")+
  ylab("Average Monthly Gift Amount ($)")+
  scale_colour_discrete(name="Gift Type")+
  theme_clean()+
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.box="horizontal",
        legend.key=element_rect(fill="white"),
        axis.text = element_text(size = 12),
        axis.title=element_text(face="bold", size= 16),
        plot.title=element_text(face = "bold", size= 24),
        legend.background =element_rect(fill="transparent", colour=NA))
 

#Raw gift amount over time with smooth lines
ggplot()+
  geom_line(data=(ira_adjusted_mean%>%
                    filter(variable=="raw")),
            aes(x=date, y=value, colour=type), size=1,alpha=0.4)+
  geom_vline(aes(xintercept =as.numeric(as.Date("2006-01-01"))),color="blue", size=1)+
  geom_smooth(
    data=(ira_adjusted_mean%>%
            filter(date>="2006-01-01")%>%
            filter(variable=="raw")),
    aes(x=date, y=value, colour=type), size=1,se=FALSE
  )+
  geom_smooth(
    data=(ira_adjusted_mean%>%
            filter(date<"2006-01-01")%>%
            filter(variable=="raw")),
    aes(x=date, y=value, colour=type), size=1,se=FALSE
  )+
  ggtitle("Examining the trends hints at\na need for a deeper analysis")+
  scale_y_continuous(labels=comma)+
  scale_x_date()+
  xlab("")+
  ylab("Average Monthly Gift Amount ($)")+
  scale_colour_discrete(name="Gift Type")+
  theme_clean()+
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.box="horizontal",
        legend.key=element_rect(fill="white"),
        axis.text = element_text(size = 12),
        axis.title=element_text(face="bold", size= 16),
        plot.title=element_text(face = "bold", size= 24),
        legend.background =element_rect(fill="transparent", colour=NA))

#Trend of Avg gift amount over time
ggplot()+
  geom_line(data=(ira_adjusted_mean%>%
                    filter(variable=="trend")),
            aes(x=date, y=value, colour=type), size=1,alpha=0.4)+
  geom_vline(aes(xintercept =as.numeric(as.Date("2006-01-01"))),color="blue", size=1)+
  geom_smooth(
    data=(ira_adjusted_mean%>%
            filter(date>="2006-01-01")%>%
            filter(variable=="trend")),
    aes(x=date, y=value, colour=type), size=1,se=FALSE
  )+
  geom_smooth(
    data=(ira_adjusted_mean%>%
            filter(date<"2006-01-01")%>%
            filter(variable=="trend")),
    aes(x=date, y=value, colour=type), size=1,se=FALSE
  )+
  ggtitle("IRA Donors do not use IRA gifts to \nsupplement their regular giving")+
  scale_y_continuous(labels=comma)+
  scale_x_date()+
  xlab("")+
  ylab("Average Monthly Gift Amount ($)")+
  scale_colour_discrete(name="Gift Type")+
  theme_clean()+
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.box="horizontal",
        legend.key=element_rect(fill="white"),
        axis.text = element_text(size = 12),
        axis.title=element_text(face="bold", size= 16),
        plot.title=element_text(face = "bold", size= 24),
        legend.background =element_rect(fill="transparent", colour=NA))

#Trend of Avg gift amount over time with campaign overlay
ggplot()+
  geom_line(data=(ira_adjusted_mean%>%
                    filter(variable=="trend")),
            aes(x=date, y=value, colour=type), size=1,alpha=0.4)+
  geom_vline(aes(xintercept =as.numeric(as.Date("2006-01-01"))),color="blue", size=1)+
  geom_rect(data=campaigndates[2:3,], 
            mapping=aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), color='grey', alpha=0.2)+
  geom_smooth(
    data=(ira_adjusted_mean%>%
            filter(date>="2006-01-01")%>%
            filter(variable=="trend")),
    aes(x=date, y=value, colour=type), size=1,se=FALSE
  )+
  geom_smooth(
    data=(ira_adjusted_mean%>%
            filter(date<"2006-01-01")%>%
            filter(variable=="trend")),
    aes(x=date, y=value, colour=type), size=1,se=FALSE
  )+
  ggtitle("IRA Donors do not use IRA gifts to \nsupplement their regular giving")+
  scale_y_continuous(labels=comma)+
  scale_x_date()+
  xlab("")+
  ylab("Average Monthly Gift Amount ($)")+
  scale_colour_discrete(name="Gift Type")+
  theme_clean()+
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.box="horizontal",
        legend.key=element_rect(fill="white"),
        axis.text = element_text(size = 12),
        axis.title=element_text(face="bold", size= 16),
        plot.title=element_text(face = "bold", size= 24),
        legend.background =element_rect(fill="transparent", colour=NA))


#Reponse to marketing line graph
#Response to marketing
ggplot()+
  geom_line(data=(ira_adjusted%>%
                    filter(variable=="raw"&
                             date>="2011-09-21")%>%
                    rename(Type=type)),
            aes(x=date, y=value, colour=Type), size=1)+
  
  geom_vline(data=ira_mailings_summary, aes(xintercept = as.numeric(date)), colour="goldenrod", size=1)+
  xlab("")+
  ylab("Number of Gifts")+
  scale_colour_discrete(name="Gift Type")+
  ggtitle("Marketing outreach has no observable impact")+
  theme_clean()+
  theme(axis.text = element_text(size = 10),
        axis.title=element_text(face="bold", size= 16),
        plot.title=element_text(face = "bold", size= 24),
        legend.key=element_rect(fill="white"),
        legend.background =element_rect(fill="transparent", colour=NA))


######Slide 1:Summary Stats#######
View(ira_adj %>%
       filter(trans_date>="2006-01-01") %>%   
  summarize(
  No_of_IRA_Donors=n_distinct(id),
  All_Giving=sum(amt),
  All_Gift_Counts=n(),
  Total_Number_of_IRA_Gifts=sum(ira_gift),
  All_IRA_Giving=sum(ira_amt),
  Percent_IRA_Amt_of_Total=(All_IRA_Giving/All_Giving)*100,
  Avg_IRA_Gift_Amt=(All_IRA_Giving/Total_Number_of_IRA_Gifts),
  Avg_IRA_Gift_Counts=(Total_Number_of_IRA_Gifts/No_of_IRA_Donors)
  )
  )



#Score Plots
ira_adj$GPS<- factor(ira_adj$gps, levels=c("Least Likely", "Less Likely",
                                           "Somewhat Likely", "More Likely", "Most Likely", ""))

ira_adj$MGS<- factor(ira_adj$mgs, levels=c("Least Likely", "Less Likely",
                                           "Somewhat Likely", "More Likely", "Most Likely", ""))


#Major Gift Score
ggplot(ira_adj%>%
         group_by(MGS)%>%
         summarize(number=n_distinct(id))%>%
         filter(MGS!=""))+
  geom_bar(aes(MGS, y=number), stat="identity")+
  xlab("Major Gift Score")+
  ylab("Number of Donors")+
  ylim(0,300)+
  ggtitle("IRA Donors are more inclined to give Major Gifts")+
  theme_clean()+
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.box="horizontal",
        legend.key=element_rect(fill="white"),
        axis.text = element_text(size = 10),
        axis.title=element_text(face="bold", size= 16),
        plot.title=element_text(face="bold", size= 24),
        legend.background =element_rect(fill="transparent", colour=NA))

#Gift Planning Score
ggplot(ira_adj%>%
         group_by(GPS)%>%
         summarize(number=n_distinct(id))%>%
         filter(GPS!=""))+
  geom_bar(aes(GPS, y=number), stat="identity")+
  xlab("Gift Planning Score")+
  ylim(0,300)+
  ylab("Number of Donors")+
  ggtitle("IRA Donors are more inclined to give Planned Gifts")+
  theme_clean()+
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.box="horizontal",
        legend.key=element_rect(fill="white"),
        axis.text = element_text(size = 10),
        axis.title=element_text(face="bold", size= 16),
        plot.title=element_text(face="bold", size= 24),
        legend.background =element_rect(fill="transparent", colour=NA))



#Count by Decile

ggplot(ira_adj%>%
         group_by(decile)%>%
         summarize(number=n_distinct(id)))+
  geom_bar(aes(decile, y=number), stat="identity")+
  ggtitle("Decile, Based On Adjusted Total Lifetime Giving")+
  ylab("Number of Donors")+
  xlab("Giving Decile")+
  theme_clean()+
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.box="horizontal",
        legend.key=element_rect(fill="white"),
        axis.text = element_text(size = 10),
        axis.title=element_text(face="bold", size= 16),
        plot.title=element_text(face="bold", size= 24),
        legend.background =element_rect(fill="transparent", colour=NA))

# Counts by gift size

ggplot(ira_adj%>%filter(ira_amt > 0 & ira_amt <= 100000)%>%select(ira_amt), aes(x=ira_amt))+
  geom_histogram(binwidth=5000)+
  xlab("Gift Amount($)")+
  ylab("Number of Gifts")+
  ggtitle("60% of IRA Gifts are under $5,000")+
  scale_x_continuous(labels = comma)+
  xlim(NA,110001)+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)+
  theme_clean()+
    theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.box="horizontal",
        legend.key=element_rect(fill="white"),
        axis.text = element_text(size = 12),
        axis.title=element_text(face="bold", size= 16),
        plot.title=element_text(face = "bold", size= 24,hjust = 0.5),
        legend.background =element_rect(fill="transparent", colour=NA))
  

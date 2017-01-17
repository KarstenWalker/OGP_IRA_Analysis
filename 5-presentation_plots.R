library(dplyr)
library(ggplot2)
library(cowplot)

#####About#####
#This file is for the first round of presentation plots.
#Please note any changes in axis', scales, or other transformations in the plot comments

#Load plotting theme
source("R:/AIM/Advanced Analytics/Functions/theme_clean.r")

#Load legend move function
source("R:/AIM/Advanced Analytics/Functions/legend_move.r")

#Avg time between gifts by transaction age (current or age at death) by IRA vs Non-IRA
#Uncomment last line to move legend onto the plot.  Sometimes works better for slides projected onto a big screen.
ggplot(ira_adj%>%
         ungroup()%>%
         filter(norm_outlier_score<.020 |
                  decile!="Top"|
                  decile !="10")%>%
         select(ira_law,age,time_between, adj_amt)%>%
         filter(age>=70.5)%>%
         mutate(age=trunc(age))%>%
         group_by(age, ira_law)%>%
         summarize(time_between=mean(time_between),
                   avg_amt=mean(adj_amt)))+
  geom_point(aes(x=age,y=time_between,color=as.factor(ira_law), size=avg_amt))+
  xlim(70.5,95)+
  xlab("Age At Time of Gift")+
  ylim(0,300)+
  ylab("Avg # of Days Between Gifts")+
  scale_color_discrete(name="IRA Law", labels=c("Before", "After"))+
  scale_size_continuous(range=c(0,10),name="Average Gift Amount",guide = guide_legend(override.aes = list(colour = "#F8766D")))+
  geom_smooth (aes(x=age,y=time_between,color=as.factor(ira_law)), se=FALSE)+
  ggtitle("Donors of All Ages Wait Longer Between Gifts,\n But Have a Larger Average Gift Amount After IRA LAW ")+
  theme_clean()+
  theme(legend.position=c(.5, .92),
        legend.direction = "horizontal",
        legend.box="horizontal",
        legend.key=element_rect(fill="white"),
        axis.text = element_text(size = 8),
        axis.title=element_text(face="bold", size= 10),
        plot.title=element_text(face = "bold", size= 12),
        legend.background =element_rect(fill="transparent", colour=NA))

#Seasonally adjusted number of gifts/number of IRA Gifts
#Both seasonally adjusted metric on one plot
ggplot()+
  geom_line(data=(ira_adjusted%>%
                    filter(variable=="raw")),
            aes(x=date, y=value, colour=type), size=1)+
  geom_vline(aes(xintercept =as.numeric(as.Date("2006-01-01"))),color="blue", size=1)+
  ylim(0,300)+
  ggtitle("IRA Giving Follows and Supplements Giving Trends")+
  scale_x_date()+
  scale_colour_discrete(name="Gift Type")+
  xlab("Year")+
  ylab("Number of Gifts")+
  theme_clean()+
  theme(axis.text = element_text(size = 8),
        axis.title=element_text(face="bold", size= 10),
        plot.title=element_text(face = "bold", size= 12),
        legend.background =element_rect(fill="transparent", colour=NA))

#Response to marketing
ggplot(ira_responses%>%
         ungroup()%>%
         filter(type != "ira_gift")%>%
         group_by(type)%>%
         summarize(responses=n())
)+
  geom_bar(aes(x=type, y=responses, fill=type), stat="identity")+
  xlab("Marketing Outreach Type")+
  ylab("Number of IRA Gifts")+
  ggtitle("Number of IRA Gifts That Directly Follow Outreach")+
  theme_clean()+
  theme(axis.text = element_text(size = 8),
        axis.title=element_text(face="bold", size= 10),
        plot.title=element_text(face = "bold", size= 12),
        legend.position = "none")

#Reponse to marketing line graph
#Response to marketing
ggplot()+
  geom_line(data=(ira_responses%>%
              ungroup()%>%
              filter(type == "ira_gift")%>%
              group_by(date)%>%
              summarize(responses=n())), aes(x=date, y=responses))+
  geom_vline(data=ira_mailings_summary, aes(xintercept = as.numeric(date), colour="green"))+
  xlab("Date")+
  ylab("Number of IRA Gifts")+
  ggtitle("Number of IRA Gifts That Directly Follow Outreach")+
  theme_clean()+
  theme(axis.text = element_text(size = 8),
        axis.title=element_text(face="bold", size= 10),
        plot.title=element_text(face = "bold", size= 12),
        legend.position = "none")
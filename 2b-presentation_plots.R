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

#Response to marketing
ggplot(ira_mailings_response%>%
         ungroup()%>%
         filter(type != "ira_gift" & type !="non_ira_gift")%>%
         group_by(type)%>%
         summarize(responses=sum(response)))+
  geom_bar(aes(x=type, y=responses), stat="identity")+
  theme_clean()



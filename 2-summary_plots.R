library(dplyr)
library(ggplot2)
library(cowplot)

#####About#####
#This file is for "summary" plots- quick plots that do not rely on
#special functions, packages, etc.  Used to quickly visualize data trends.
#Please note any changes in axis', scales, or other transformations in the plot comments

#Load plotting theme
source("R:/AIM/Advanced Analytics/Functions/theme_clean.r")

#Load legend move function
source("R:/AIM/Advanced Analytics/Functions/legend_move.r")

#####Number of gifts#####
#Normal curves
ggplot(pre_post_id, aes(num_gifts, fill=ira_law))+
  xlim(0,100)+
  theme_clean()+
  stat_function(fun=dnorm,
                color="light blue", args=list(mean=mean(pre_post_id$num_gifts[pre_post_id$ira_law =="0"],
                                                 sd=sd(pre_post_id$num_gifts))))+
  stat_function(fun=dnorm,
                color="light green", args=list(mean=mean(pre_post_id$num_gifts[pre_post_id$ira_law =="1"],
                                                  sd=sd(pre_post_id$num_gifts))))

#Density plot number of gifts
#Can play with xlim to eliminate the few cases of extreme number of gifts
ggplot(ira_adj%>%
         select(id, ira_law)%>%
         group_by(id, ira_law)%>%
         summarize(num_gifts=n())
)+
  geom_density(aes(num_gifts, fill=ira_law),alpha=.2)+
  xlim(0,100)+
  theme_clean()

#Count histogram
#Can play with xlim to eliminate the few cases of extreme number of gifts
ggplot(ira_adj%>%
         select(id, ira_law)%>%
         group_by(id, ira_law)%>%
         summarize(num_gifts=n())
       )+
  geom_histogram(aes(num_gifts, fill=ira_law), alpha=.2, binwidth = 1)+
 # xlim(0,100)+
  theme_clean()

#Normal curve superimposed over density plot or histogram for all donors
#Uncomment lines to change plot limits and plot type
ggplot(pre_post_id, aes(num_gifts, fill=ira_law))+
  geom_histogram(binwidth=1,alpha=.2, aes(y=..density..))+
  geom_density(aes(num_gifts, color=ira_law),alpha=.2)+
  xlim(0,100)+
  theme_clean()+
  stat_function(fun=dnorm,
                           color="red", args=list(mean=mean(pre_post_id$num_gifts[pre_post_id$ira_law =="0"],
                                                            sd=sd(pre_post_id$num_gifts))))+
  stat_function(fun=dnorm,
                color="blue", args=list(mean=mean(pre_post_id$num_gifts[pre_post_id$ira_law =="1"],
                                                 sd=sd(pre_post_id$num_gifts))))+
  theme_clean()

#Density plots superimposed over histogram
ggplot(pre_post_id, aes(num_gifts, fill=ira_law))+
  geom_histogram(binwidth=1,alpha=.2, aes(y=..density..))+
  geom_density(aes(num_gifts, color=ira_law),alpha=.2)+
  xlim(0,100)+
  theme_clean()

#Outlier score histogram
ggplot(id_summary, aes(x=norm_outlier_score))+
  #geom_histogram(binwidth=.1, alpha=.2, aes(y=..density..))+
  geom_density()+
  xlim(0,.025)+
  theme_clean()

########Pre/Post Law Giving########

#####Giving by Decile#####
#Change in mean amount by decile
ggplot(ira_adj)+
  geom_point(aes(x=mean_amt_change, y=adj_total_giving_change, color=as.factor(ira_law)))+
  facet_wrap(~decile, scales="free")+
  theme_clean()

#Change in time between gifts versus change in mean gift amount
ggplot(id_summary)+
  geom_point(aes(x=pre_post_mean_change, y=frequency_change))+
  facet_wrap(~decile, scales="free")+
  theme_clean()

#Change in time between gifts versus change in mean gift amount
#using colors for pre/post
ggplot(id_summary)+
  geom_point(aes(x=pre_law_mean_amt, y=pre_time_between),colour=
               "tomato1")+
  geom_point(aes(x=post_law_mean_amt, y=post_time_between), colour="palegreen2")+
  facet_wrap(~decile, scales="free")+
  theme_clean()

#####Giving by Age#####
# Adjusted total giving by transaction age (current or age at death) by IRA vs Non-IRA
ggplot(ira_adj%>%
  select(id, ira_law,age,adj_amt, nbr)%>%
  group_by(id, ira_law, age)%>%
    summarize(age_total=sum(adj_amt),
              num_gifts=n_distinct(nbr)))+
  geom_point(aes(x=age,y=age_total,color=as.factor(ira_law),size=num_gifts))+
  xlim(70.5,95)+
  ylim(0,25000)+
  geom_smooth(method="lm", aes(x=age,y=age_total,color=as.factor(ira_law)))+
  #scale_y_log10()+
  theme_clean()

#Same plot, but faceted by age_bracket
ggplot(id_age_summary<- ira_adj%>%
         filter(norm_outlier_score<.020 |
                  decile!="Top"|
                  decile !="10")%>%
         ungroup()%>%
         select(id, ira_law, gift_age_bracket,adj_amt, time_between)%>%
         group_by(id, ira_law, gift_age_bracket)%>%
         mutate(mean_amt=mean(adj_amt),
                mean_time_between=mean(time_between))%>%
         group_by(gift_age_bracket)%>%
         filter(mean_amt<=(.3*max(mean_amt))))+
  geom_point(aes(x=mean_amt, y=mean_time_between, color=as.factor(ira_law)))+
  facet_wrap(~gift_age_bracket)+
  theme_clean()

#By trans_bin
ggplot(id_age_summary<- ira_adj%>%
         filter(norm_outlier_score<.020 |
                  decile!="Top"|
                  decile !="10")%>%
         ungroup()%>%
         select(id, ira_law, trans_bin, adj_amt, time_between)%>%
         group_by(id, ira_law, trans_bin)%>%
         mutate(mean_amt=mean(adj_amt),
                mean_time_between=mean(time_between))%>%
         ungroup()%>%
         group_by(trans_bin)%>%
         filter(mean_amt<=(.3*max(mean_amt))))+
  geom_point(aes(x=mean_amt, y=mean_time_between, color=as.factor(ira_law)))+
  facet_wrap(~trans_bin, scales="free")+
  theme_clean()

#Avg time between gifts by transaction age (current or age at death) by IRA vs Non-IRA
#Uncomment last line to move legend onto the plot.  Sometimes works better for slides projected onto a big screen.
ggplot(ira_adj%>%
         ungroup()%>%
         filter(norm_outlier_score<.020 |
                  decile!="Top"|
                  decile !="10")%>%
         select(ira_law,age,time_between, nbr)%>%
         filter(age>=70.5)%>%
         mutate(age=trunc(age))%>%
         group_by(age, ira_law)%>%
         summarize(time_between=mean(time_between),
                   gift_num=n_distinct(nbr)))+
  geom_point(aes(x=age,y=time_between,color=as.factor(ira_law), size=gift_num))+
  xlim(70.5,95)+
  xlab("Age At Time of Gift")+
  ylim(0,300)+
  ylab("Avg # of Days Between Gifts")+
  scale_color_discrete(name="IRA Law", labels=c("Before", "After"))+
  scale_size_continuous(range=c(0,10),name="Number of Gifts",guide = guide_legend(override.aes = list(colour = "#F8766D")))+
  geom_smooth (aes(x=age,y=time_between,color=as.factor(ira_law)), se=FALSE)+
  ggtitle("Donors of All Ages Wait Longer Between Gifts,\n But Give More Gifts After IRA LAW ")+
  theme_clean()+
  theme(legend.position=c(.91, .87),
        axis.text = element_text(size = 8),
        axis.title=element_text(face="bold", size= 10),
        plot.title=element_text(face = "bold", size= 12),
        legend.background =element_rect(fill="transparent", colour=NA))

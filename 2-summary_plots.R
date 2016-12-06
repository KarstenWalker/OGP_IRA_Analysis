library(dplyr)
library(ggplot2)

#####About#####
#This file is for "summary" plots- quick plots that do not rely on
#special functions, packages, etc.  Used to quickly visualize data trends.
#Please note any changes in axis', scales, or other transformations in the plot comments

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

#####Number of gifts#####
#Normal curves
ggplot(pre_post_id, aes(num_gifts, fill=ira_law))+
  xlim(0,100)+
  theme_kw+
  stat_function(fun=dnorm,
                fill="red", args=list(mean=mean(pre_post_id$num_gifts[pre_post_id$ira_law =="0"],
                                                 sd=sd(pre_post_id$num_gifts))))+
  stat_function(fun=dnorm,
                color="green", args=list(mean=mean(pre_post_id$num_gifts[pre_post_id$ira_law =="1"],
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
  theme_kw

#Count histogram
#Can play with xlim to eliminate the few cases of extreme number of gifts
ggplot(ira_adj%>%
         select(id, ira_law)%>%
         group_by(id, ira_law)%>%
         summarize(num_gifts=n())
       )+
  geom_histogram(aes(num_gifts, fill=ira_law), alpha=.2, binwidth = 1)+
 # xlim(0,100)+
  theme_kw

#Normal curve superimposed over density plot or histogram for all donors
#Uncomment lines to change plot limits and plot type
ggplot(pre_post_id, aes(num_gifts, fill=ira_law))+
  geom_histogram(binwidth=1,alpha=.2, aes(y=..density..))+
  geom_density(aes(num_gifts, color=ira_law),alpha=.2)+
  xlim(0,100)+
  theme_kw+
  stat_function(fun=dnorm,
                           color="red", args=list(mean=mean(pre_post_id$num_gifts[pre_post_id$ira_law =="0"],
                                                            sd=sd(pre_post_id$num_gifts))))+
  stat_function(fun=dnorm,
                color="blue", args=list(mean=mean(pre_post_id$num_gifts[pre_post_id$ira_law =="1"],
                                                 sd=sd(pre_post_id$num_gifts))))+
  theme_kw



#Density plots superimposed over histogram
ggplot(pre_post_id, aes(num_gifts, fill=ira_law))+
  geom_histogram(binwidth=1,alpha=.2, aes(y=..density..))+
  geom_density(aes(num_gifts, color=ira_law),alpha=.2)+
  xlim(0,100)+
  theme_kw

#####Number of gifts#####

ggplot(ira_adj, aes(norm_outlier_score))+
  geom_histogram(binwidth=.01, alpha=.2, aes(y=..density..))+
  geom_density()+
  xlim(0,.25)+
  theme_kw

# Adjusted total giving by age (current or age at death) by IRA vs Non-IRA
plot_df<-ira_adj%>%
  select(id,gender_code,ira_donor,age,num_gifts,adj_total_giving)%>%
  group_by(id,gender_code,ira_donor,age)%>%distinct(.keep_all=TRUE)

ggplot(plot_df,aes(x=age,y=adj_total_giving,color=ira_donor))+
  geom_point(aes(age,color=ira_donor))+
  xlim(55,110)+
  ylim(0,800000)+
  geom_smooth(method="lm")+
  facet_wrap(~ira_donor)

#Yearlty total giving by IRA donor vs Non IRA donor, before and after the change in law.
plot_df1<-ira_adj%>%
  select(id,year,year_total,ira_law,ira_donor)%>%
  distinct(.keep_all=TRUE)


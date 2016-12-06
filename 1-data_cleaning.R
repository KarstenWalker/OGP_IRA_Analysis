library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(zoo)
library(chron)
#System Date
date <-as.Date(Sys.Date())

#####About#####
#This file is for basic data importation, cleaning, and munging.  Any model
#preparation, formatting for plotting, function creation, or function loading
#should be done in the appropriate files. If you are repeatedly using a dataframe,
#matrix, or list that is not in this file, please add and include the necessary
#details in the comments.  These main tables should provide the basis for all necessary
#summary tables i.e. if you want number of IRA gifts by month, use the ira_yearmon table as your basis.

#Read in .csv
ira_data<- read.csv("R:/AIM/Advanced Analytics/OGP_IRA/ira_data.csv",
                header=TRUE,
                stringsAsFactors=FALSE
)

#Load year format function
source("R:/AIM/Advanced Analytics/Functions/year_format.r")

#Format dates
ira_data$trans_date <- dmy(ira_data$trans_date)
ira_data$year <- year(ira_data$trans_date)
ira_data$birth_date<-dmy(ira_data$birth_date)
ira_data$death_date<-dmy(ira_data$death_date)

#Prim purp roll-up

student_support <- c("GSS", "BSS", "USS")

ira_data$prim_purp <- replace(ira_data$prim_purp, 
                                 ira_data$prim_purp %in% student_support,
                                 "STU")

dept_support <- c("CHR", "LIB", "UNR", "UNU")

ira_data$prim_purp <- replace(ira_data$prim_purp, 
                                 ira_data$prim_purp %in% dept_support,
                                 "DEP")

other_purp <- c("PND", "DRC", "NIS", "MUL")

ira_data$prim_purp <- replace(ira_data$prim_purp, 
                                 ira_data$prim_purp %in% other_purp,
                                 "OTH")

#Load inflation adjustment functions
source("R:/AIM/Advanced Analytics/Functions/inflation_functions.r")
lookup<- inflation_lookup(2016)
adjust2016<- inflation_adjuster(2016)

#Campaign Dates
start <- as.Date(c("1986-07-01", "1996-07-01", "2005-07-01"))
end <- as.Date(c("1990-07-01", "2000-12-31", "2013-12-31"))

campaigndates<- data.frame(start=as.Date(start),
                           end=as.Date(end))

#Adjust transactions for inflation, create summary statistics. yeardate column is for better
#x-axis plotting
ira_adj <- ira_data %>%
  filter(person=="P")%>%
  group_by(id)%>%
  mutate(age=as.numeric((difftime((as.Date(Sys.Date())), birth_date, units="weeks"))/52),
         years_since_death=as.numeric((difftime((as.Date(Sys.Date())), death_date, units="weeks"))/52),
         est_age_cc=2016-class_year+23)%>%
  ungroup()%>%
  mutate(yeardate=floor_date(trans_date, unit="year"),
         yearmon=floor_date(trans_date, unit="month"))%>%
  group_by(year)%>%
  mutate(adjustment=adjust2016(year))%>%
  ungroup()%>%
  group_by(id) %>%
  arrange(trans_date, nbr)%>%
  mutate(first_trans_date=first(trans_date),
         gift_num=as.numeric(row_number()),
         num_ira_gifts=sum(ira_gift),
         ira_donor=ifelse(sum(ira_gift)>=1,1,0),
         first_ira=ira_gift==1 & !duplicated(ira_gift==1),
         first_ira=replace(first_ira, which(first_ira=='TRUE'),1),
         ira_law=ifelse(year>=2006, 1,0),
         pre_law_gift=ifelse(ira_law==0,1,0),
         num_pre_law=sum(ifelse(ira_law==0,1,0)),
         post_law_gift=ifelse(ira_law==1,1,0),
         num_post_law=sum(ifelse(ira_law==1,1,0)),
         post_law_ira=sum(ifelse(ira_law==1 &ira_gift==1,1,0)),
         total_giving=sum(amt),
         adj_amt=amt/adjustment,
         adj_total_giving=sum(adj_amt),
         amt_change=adj_amt-lag(adj_amt),
         mean_amt=mean(adj_amt),
         median_amt=median(amt),
         max_amt=max(adj_amt),
         num_gifts= n(),
         repeat_ira_donor=ifelse(num_ira_gifts>1, 1, 0),
         first_amt=first(adj_amt),
         last_amt=last(adj_amt),
         larger_gift=ifelse(amt_change>0, 1, 0),
         num_larger=sum(larger_gift, na.rm=TRUE),
         time_between=as.numeric(difftime(trans_date, lag(trans_date), units="days")),
         time_between=replace(time_between, which(is.na(time_between)),0),
         time_from_first=as.numeric(difftime(trans_date, first_trans_date, units="days")),
         avg_time_between=mean(time_between),
         total_giving_span=max(time_from_first),
         total_giving_span=replace(total_giving_span, which(is.na(total_giving_span)),1),
         pct_giving_span=time_from_first/total_giving_span)%>%
  ungroup()%>%
  group_by(id)%>%
  arrange(year)%>%
  mutate(year_total=sum(adj_amt),
         year_total_change=year_total-lag(year_total),
         year_total_pct=(year_total/lag(year_total)-1),
         year_mean=mean(adj_amt),
         year_mean_change=year_mean-lag(year_mean),
         year_mean_pct=(year_mean/lag(year_mean)-1),
         year_med=median(adj_amt),
         year_med_change=year_med-lag(year_med),
         year_gifts=n(),
         year_gift_change=year_gifts-lag(year_gifts),
         year_gift_pct=(year_gifts/lag(year_gifts)-1),
         year_max=max(adj_amt),
         year_num_ira=sum(ira_gift),
         year_num_larger=sum(num_larger, na.rm=TRUE)
  )%>%
  arrange(gift_num)%>%
  mutate(span_quartile=ifelse(pct_giving_span>=0 & pct_giving_span<=.25, 1,
                              ifelse(pct_giving_span>.25 & pct_giving_span<=.5, 2,
                                     ifelse(pct_giving_span>.5 & pct_giving_span<=.75, 3, 4)
                              )),
         span_quartile=replace(span_quartile, which(is.na(span_quartile)),5))%>%
  ungroup()%>%
  mutate(total_giving_span=replace(total_giving_span, which(total_giving_span==0),1),
         outlier_score=((total_giving*num_gifts)/total_giving_span),
         norm_outlier_score=(outlier_score-min(outlier_score))/(max(outlier_score)-min(outlier_score)))

#Create data.frame of ira_donors with no age and estimate based on first IRA gift date
ira_ageless<-ira_adj%>%
  select(id, ira_donor, first_ira, age, trans_date)%>%
  filter(ira_donor==1 &is.na(age))%>%
  select(-age)%>%
  group_by(id)%>%
  arrange(desc(first_ira))%>%
  mutate(first_ira_date=paste(first(trans_date)),
         time_since_first=as.numeric((difftime((as.Date(Sys.Date())), first_ira_date, units="weeks"))/52),
         est_age=time_since_first+70.5)%>%
  select(id, est_age)%>%
  distinct(.keep_all = TRUE)

#Join the estimated age
ira_adj<-ira_adj%>%
  left_join(ira_ageless, by="id")

#Remove useless data.frame from environment
rm(list='ira_ageless')

#Load move_column function
source("R:/AIM/Advanced Analytics/Functions/move_column.r")

#Move estimated age column to be next to other age columns
ira_adj<-move_column(ira_adj, c("est_age"), "after", "age")%>%mutate(age=age-years_since_death,
                                                                     est_age=est_age-years_since_death,
                                                                     est_age_cc=est_age_cc-years_since_death)
ira_adj<-move_column(ira_adj, c("death_date"), "before", "years_since_death")
ira_adj<-move_column(ira_adj, c("birth_date"), "before", "age")
ira_adj<-move_column(ira_adj, c("est_age_cc"), "after", "est_age")
ira_adj<-move_column(ira_adj, c("class_year"), "after", "birth_date")


  
#Add deciles based on dataset
ira_adj<-ira_adj%>%
  mutate(decile= with(ira_adj, factor(findInterval(adj_total_giving, 
                                                      c(-Inf,
                                                        quantile(adj_total_giving,c(.1, .2, .3, .4, .5, .6, .7, .8, .9, .995)),Inf)),
                                         labels=c("1","2","3","4","5", "6", "7", "8", "9", "10", "Top"))))
#Import ID/decile table
#Deciles are calculated based on an inflation-adjusted lifetime giving total
#for all donors who have given over $1 to UC Berkeley.
#Since we have calculated deciles for just this dataset, we will relabel the CADS deciles
deciles<- read.csv("R:/AIM/Advanced Analytics/Functions/deciles.csv",
                   header=TRUE,
                   stringsAsFactors=FALSE,
                   colClasses=c("integer", "factor")
)

deciles<-deciles%>%rename(CADS_decile=decile)

#Join to summary table
ira_adj<-ira_adj%>%
  left_join(deciles, by="id")
rm(list='deciles')

#IRA gift data.frame for quick viewing
ira_gifts<-ira_adj%>%filter(ira_gift==1)

#Pre law 
pre_law<- ira_adj%>%
  filter(ira_law==0)%>%
  select(id, adj_amt, larger_gift, time_between)%>%
  group_by(id)%>%
  summarize(pre_law_mean=mean(adj_amt),
         pre_law_median=median(adj_amt),
         sum_pre_law=sum(adj_amt),
         max_pre_law=max(adj_amt),
         pre_law_larger=sum(larger_gift, na.rm=TRUE),
         pre_time_between=mean(time_between))%>%
  ungroup()

#Post law
post_law<-ira_adj%>%
  filter(ira_law==1)%>%
  select(id, adj_amt, larger_gift, time_between)%>%
  group_by(id)%>%
  summarize(post_law_mean=mean(adj_amt),
         post_law_median=median(adj_amt),
         sum_post_law=sum(adj_amt),
         max_post_law=max(adj_amt),
         post_law_larger=sum(larger_gift, na.rm=TRUE),
         post_time_between=mean(time_between))%>%
  ungroup()

#Join by ID
ira_adj<-ira_adj%>%
  left_join(pre_law, by='id')%>%
  left_join(post_law, by='id')%>%
  ungroup()

#Per ID Summary
year_vars<-ira_adj%>%
  select(starts_with("year"))

amt_vars<-ira_adj%>%
  select(-amt, -adj_amt,-amt_change)

id_summary<- ira_adj%>%
  ungroup()%>%
  select(-amt, -adj_amt, -ira_law, -ira_gift,
         -gift_num, -pre_law_gift, -post_law_gift,
         -amt_change, -larger_gift, -time_between,
         -time_from_first, -span_quartile)%>%
  group_by(id)%>%
  mutate(num_ira=max(num_ira_gifts))%>%
  filter(row_number(id)==1)

#Pre/post summary for plotting.
ira_adj$ira_law<-as.factor(ira_adj$ira_law)
pre_post_id<- ira_adj%>%
  select(id, ira_law)%>%
  ungroup()%>%
  group_by(id, ira_law)%>%
  summarise(num_gifts=n())

#Year/month summary.Useful for time series conversion and plotting.
#Daily data is usually too granular
ira_yearmon<-ira_adj%>%
  ungroup()%>%
  filter(norm_outlier_score <.10)%>%
  group_by(yearmon)%>%
  summarize(num_donors=n_distinct(id),
            num_ira_donors=sum(ifelse(row_number(id)==1 & num_ira_gifts>=1,1,0)),
            num_gifts= n(),
            num_ira_gifts=sum(ira_gift),
            num_first_ira=sum(first_ira),
            total_giving=sum(adj_amt),
            mean_amt=mean(adj_amt),
            median_amt=median(adj_amt),
            max_amt=max(adj_amt),
            avg_time_between=mean(time_between))%>%
  mutate(donor_change=num_donors-lag(num_donors),
         pct_donor_change=(num_donors/lag(num_donors)-1),
         gift_change=num_gifts-lag(num_gifts),
         pct_gift_change=(num_gifts/lag(num_gifts)-1),
         ira_donor_change=num_ira_donors-lag(num_ira_donors),
         prop_ira_donor_change=(num_ira_donors/lag(num_ira_donors)-1),
         prop_ira_donors=num_ira_donors/num_donors,
         prop_ira_gifts=num_ira_gifts/num_gifts,
         prop_ira_gift_change=prop_ira_gifts-lag(prop_ira_gifts),
         total_giving_change=total_giving-lag(total_giving),
         pct_giving_change=(total_giving/lag(total_giving)-1))

#Year summary.Could be useful for reporting.
ira_year<-ira_adj%>%
  ungroup()%>%
  group_by(year)%>%
  summarize(num_donors=n_distinct(id),
            num_ira_donors=sum(ifelse(row_number(id)==1 & num_ira_gifts>=1,1,0)),
            num_gifts= n(),
            num_ira_gifts=sum(ira_gift),
            num_first_ira=sum(first_ira),
            total_giving=sum(adj_amt),
            mean_amt=mean(adj_amt),
            median_amt=median(adj_amt),
            max_amt=max(adj_amt),
            avg_time_between=mean(time_between))%>%
  mutate(donor_change=num_donors-lag(num_donors),
         pct_donor_change=(num_donors/lag(num_donors)-1),
         gift_change=num_gifts-lag(num_gifts),
         pct_gift_change=(num_gifts/lag(num_gifts)-1),
         ira_donor_change=num_ira_donors-lag(num_ira_donors),
         prop_ira_donor_change=(num_ira_donors/lag(num_ira_donors)-1),
         prop_ira_donors=num_ira_donors/num_donors,
         prop_ira_gifts=num_ira_gifts/num_gifts,
         prop_ira_gift_change=prop_ira_gifts-lag(prop_ira_gifts),
         total_giving_change=total_giving-lag(total_giving),
         pct_giving_change=(total_giving/lag(total_giving)-1))

         
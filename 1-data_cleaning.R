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
ira_data<- read.csv("R:/AIM/Advanced Analytics/OGP_IRA/ira_new.csv",
                    header=TRUE,
                    stringsAsFactors=FALSE
)

#Read in mailing data
ira_mailings<- read.csv("R:/AIM/Advanced Analytics/OGP_IRA/mailing_list.csv",
                        header=TRUE,
                        stringsAsFactors=FALSE
)

names(ira_mailings)<- tolower(names(ira_mailings))

ira_mailings<-ira_mailings%>%
  rename(touch=marketing.touch.name,
         date=marketing.touch.date,
         id=cadsid)%>%
  mutate(touch=gsub(" ", "\n",touch))

ira_mailings$type<-tolower(ira_mailings$type)
ira_mailings$touch<- tolower(ira_mailings$touch)

#Load year format function
source("R:/AIM/Advanced Analytics/Functions/year_format.r")

#Format dates
ira_data$trans_date <- dmy(ira_data$trans_date)
ira_data$year <- year(ira_data$trans_date)
ira_data$birth_date<-dmy(ira_data$birth_date)
ira_data$death_date<-dmy(ira_data$death_date)
ira_mailings$date= mdy(ira_mailings$date)

#Create mailing summary data frame
ira_mailings_summary<- ira_mailings%>%
  group_by(date, type, touch)%>%
  summarize(donors=n_distinct(id))

#Per ID solicitations
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
#Campaign Dates
start <- as.Date(c("1986-07-01", "1996-07-01", "2005-07-01"))
end <- as.Date(c("1990-07-01", "2000-12-31", "2013-12-31"))

campaigndates<- data.frame(start=as.Date(start),
                           end=as.Date(end))

#IRA Mailing Dates
dates<- as.Date(c("2016-07-29", "2016-09-06", "2016-11-28",
                  "2015-12-21", "2013-11-12", "2013-01-04",
                  "2012-11-09", "2011-09-21", "2011-09-21",
                  "2010-01-04"))

appeals<- as.character(c("Donor Letter", "Postcard", "HTML", 
                         "HTML", "Postcard", "HTML", "Donor Letter", 
                         "Postcard", "Letter", "Postcard"))

num_appeals<- as.numeric(c(150, 40000, 40000,
                           570, 25000, 9523,
                           150, 29496, 34,
                           30865))

ira_appeals<- data.frame(dates, appeals, num_appeals)

#Adjust transactions for inflation, create summary statistics. yeardate column is for better
#x-axis plotting
ira_adj <- ira_data %>%
  ungroup()%>%
  filter(person=="P")%>%
  group_by(id)%>%
  mutate(age_birth=as.numeric((difftime((as.Date(Sys.Date())), birth_date, units="weeks"))/52),
         years_since_death=as.numeric((difftime((as.Date(Sys.Date())), death_date, units="weeks"))/52),
         years_since_death=ifelse(is.na(years_since_death),0,years_since_death),
         age_cc=2017-class_year+23)%>%
  ungroup()%>%
  mutate(yeardate=floor_date(trans_date, unit="year"),
         yearmon=floor_date(trans_date, unit="month"))%>%
  group_by(year)%>%
  mutate(adjustment=adjust2016(year))%>%
  ungroup()%>%
  group_by(id) %>%
  arrange(trans_date, nbr)%>%
  mutate(trans_age_birth=as.numeric((difftime(trans_date, birth_date, units="weeks"))/52),
         time_since_trans=as.numeric((difftime((as.Date(Sys.Date())), trans_date, units="weeks"))/52),
         trans_age_cc=age_cc-time_since_trans,
         ira_eligible=ifelse(trans_age_birth >=70.5,1,0),
         first_trans_date=first(trans_date),
         gift_num=as.numeric(row_number()),
         num_gift=1,
         num_ira_gifts=sum(ira_gift),
         ira_donor=as.factor(ifelse(sum(ira_gift)>=1,1,0)),
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
         ira_amt=as.numeric(ifelse(ira_gift==1, paste(as.character(amt)),"0")),
         adj_ira_amt=as.numeric(ifelse(ira_gift==1, paste(as.character(adj_amt)),"0")),
         adj_ira_amt_change=adj_ira_amt-lag(adj_ira_amt),
         ira_total=ifelse(ira_gift==1, sum(ira_amt),0),
         ira_total=replace(ira_total, which(ira_total==0),max(ira_total)),
         adj_ira_total=ifelse(ira_gift==1, sum(adj_ira_amt),0),
         adj_ira_total=replace(adj_ira_total, which(adj_ira_total==0),max(adj_ira_total)),
         mean_ira_amt=mean(ira_amt),
         adj_total_giving=sum(adj_amt),
         adj_non_ira_giving=adj_total_giving-adj_ira_total,
         non_ira_amt=amt-ira_amt,
         adj_non_ira_amt=adj_amt-adj_ira_amt,
         non_ira_giving=total_giving-ira_total,
         adj_total_roll=cumsum(adj_amt),
         adj_total_giving_change=adj_total_roll-lag(adj_total_roll),
         amt_change=adj_amt-lag(adj_amt),
         mean_amt=mean(adj_amt),
         roll_mean=cummean(adj_amt),
         mean_amt_change=roll_mean-lag(roll_mean),
         median_amt=median(amt),
         max_amt=max(adj_amt),
         num_gifts= n(),
         num_non_ira_gifts=num_gift-ira_gift,
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
  arrange(yearmon)%>%
  mutate(eligible=ifelse(ira_eligible==1, 1, 0),
         yearmon_eligible_flg=if_else(row_number(id)==1 & eligible==1,1,0))%>%
  select(-eligible)%>%
  filter(ira_donor==1)%>%
  group_by(id)%>%
  arrange(year)%>%
  mutate(year_total=sum(adj_amt),
         year_total_change=year_total-lag(year_total),
         year_total_pct=(year_total/lag(year_total)-1),
         year_ira_total=sum(ira_amt),
         year_ira_total_change=year_ira_total-lag(year_ira_total),
         year_ira_total_pct=(year_ira_total/lag(year_ira_total)-1),
         year_ira_prop=year_ira_total/100000,
         year_ira_gifts=sum(ira_gift),
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
         year_num_larger=sum(num_larger, na.rm=TRUE))%>%
  arrange(gift_num)%>%
  mutate(span_quartile=ifelse(pct_giving_span>=0 & pct_giving_span<=.25, 1,
                              ifelse(pct_giving_span>.25 & pct_giving_span<=.5, 2,
                                     ifelse(pct_giving_span>.5 & pct_giving_span<=.75, 3, 4)
                              )),
         span_quartile=replace(span_quartile, which(is.na(span_quartile)),5))%>%
  ungroup()%>%
  mutate(total_giving_span=replace(total_giving_span, which(total_giving_span==0),1),
         outlier_score=(total_giving*num_gifts)/total_giving_span,
         norm_outlier_score=(outlier_score-min(outlier_score))/(max(outlier_score)-min(outlier_score)))

#Create data.frame of ira_donors with no age and estimate based on first IRA gift date
ira_ageless<-ira_adj%>%
  select(id, ira_donor, first_ira, age_birth, trans_date)%>%
  filter(ira_donor==1 &is.na(age_birth))%>%
  select(-age_birth)%>%
  group_by(id)%>%
  arrange(desc(first_ira))%>%
  mutate(first_ira_date=paste(first(trans_date)),
         time_since_first=as.numeric((difftime((as.Date(Sys.Date())), first_ira_date, units="weeks"))/52),
         age_first_ira=time_since_first+70.5)%>%
  select(id, age_first_ira)%>%
  distinct(.keep_all = TRUE)

#Join the estimated age
ira_adj<-ira_adj%>%
  left_join(ira_ageless, by="id")

#Remove useless data.frame from environment
rm(list='ira_ageless')

#Load move_column function
source("R:/AIM/Advanced Analytics/Functions/move_column.r")

#Move estimated age column to be next to other age columns
#Calculate age at death for deceased. Derive final age based on hierarchy 1.age 2.est_age_cc 3.est_age
#Reorder birth, death and age related columns so they appear together.
#Create age bracket bins
ira_adj<-ira_adj%>%
  move_column(c("age_first_ira"), "after", "age_cc")%>%
  mutate(age_birth=age_birth-years_since_death,
         age_first_ira=age_first_ira-years_since_death,
         age_cc=age_cc-years_since_death,age=coalesce(age_birth,age_cc,age_first_ira))%>%
  move_column(c("death_date"), "before", "years_since_death")%>%move_column(c("birth_date"), "before", "age_birth")%>%
  move_column(c("class_year"), "after", "birth_date")%>%
  move_column(c("age_birth"), "before", "age_cc")%>%
  move_column(c("age"), "after", "age_first_ira")%>%
  move_column(c("ira_eligible"), "after", "trans_date")%>%
  ungroup()%>%
  mutate(gift_age_bracket=ifelse(age<70.5, "<70.5",
                                 ifelse(age>=70.5 & age<75, "70.5-75",
                                        ifelse(age>=75 & age<80, "75-80",
                                               ifelse(age>=80 & age<85, "80-85", "85+")
                                        ))))%>%
  mutate(trans_bin=ntile(age, 8),
         trans_rank=percent_rank(age),
         cume_rank=cume_dist(age))

#Add deciles based on dataset and deciles for ira gifts
ira_adj<-ira_adj%>%
  mutate(decile= with(ira_adj, factor(findInterval(adj_total_giving, 
                                                   c(-Inf,
                                                     quantile(adj_total_giving,c(.1, .2, .3, .4, .5, .6, .7, .8, .9, .995)),Inf)),
                                      labels=c("1","2","3","4","5", "6", "7", "8", "9", "10", "Top"))))

ira_deciles<-ira_adj%>%
  ungroup()%>%
  filter(ira_donor==1)%>%
  mutate(ira_decile= with(ira_adj, factor(findInterval(adj_ira_total, 
                                                       c(-Inf,
                                                         quantile(adj_ira_total,c(.1, .2, .3, .4, .5, .6, .7, .8, .9, .995)),Inf)),
                                          labels=c("1","2","3","4","5", "6", "7", "8", "9", "10", "Top"))))%>%
  select(id, ira_decile)

ira_adj<-ira_adj%>%
  ungroup()%>%
  left_join(ira_deciles, by="id")%>%
  distinct(.keep_all = TRUE)

#Remove ira_decile data.frame

rm(list='ira_deciles')

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

#Pre law 
pre_law<- ira_adj%>%
  filter(ira_law==0)%>%
  select(id, adj_amt, larger_gift, time_between, mean_amt_change)%>%
  group_by(id)%>%
  summarize(pre_law_mean_amt=mean(adj_amt),
            pre_law_mean_var=mean(mean_amt_change),
            pre_law_adj_total=sum(adj_amt),
            max_pre_law=max(adj_amt),
            pre_law_larger=sum(larger_gift, na.rm=TRUE),
            pre_time_between=mean(time_between))%>%
  ungroup()

#Post law
post_law<-ira_adj%>%
  filter(ira_law==1)%>%
  select(id, adj_amt, larger_gift, time_between, mean_amt_change)%>%
  group_by(id)%>%
  summarize(post_law_mean_amt=mean(adj_amt),
            post_law_mean_var=mean(mean_amt_change),
            post_law_adj_total=sum(adj_amt),
            max_post_law=max(adj_amt),
            post_law_larger=sum(larger_gift, na.rm=TRUE),
            post_time_between=mean(time_between))%>%
  ungroup()

#Join by ID
ira_adj<-ira_adj%>%
  left_join(pre_law, by='id')%>%
  left_join(post_law, by='id')%>%
  ungroup()%>%
  group_by(id)%>%
  mutate(pre_post_mean_change=post_law_mean_amt-pre_law_mean_amt,
         pre_post_mean_var=post_law_mean_var-pre_law_mean_var,
         pre_post_max_change=max_post_law-max_pre_law,
         pre_post_larger=post_law_larger-pre_law_larger,
         frequency_change=post_time_between-pre_time_between)%>%
  ungroup()

#Mailings per ID
ira_mailings_response<- ira_mailings%>%
  select(id, date, type)%>%
  bind_rows((ira_adj%>%
               select(id, trans_date, ira_gift)%>%
               group_by(id)%>%
               filter(ira_gift==1)%>%
               mutate(type=ifelse(ira_gift==1, paste("ira_gift"),0))%>%
               select(-ira_gift)%>%
               rename(date=trans_date)%>%
               filter(date>"2011-09-21")
  )
  )%>%
  group_by(id)%>%
  arrange(date)%>%
  mutate(response=ifelse(type=="ira_gift",1,0),
         respondent=ifelse(sum(response)>=1,1,0))%>%
  filter(respondent==1)%>%
  left_join((ira_adj%>%
               group_by(id)%>%
               ungroup()%>%
               filter(ira_gift==1)%>%
               select(id, trans_date, adj_ira_amt)%>%
               rename(date=trans_date)), by=c("id","date")
  )%>%
  group_by(id)%>%
  arrange(date)%>%
  mutate(response_time=as.numeric(
    ifelse(
      response==1, paste0(
        as.numeric(
          difftime(
            date, lag(date), units="days")
        )
      ), NA)
  )
  )

ira_responses<-ira_mailings_response%>%
  group_by(id)%>%
  group_by(id, date, type)%>%
  summarize(date_amt=sum(adj_ira_amt),
            response_time=sum(response_time, na.rm = TRUE))%>%
  ungroup()%>%
  group_by(id)%>%
  arrange(date)%>%
  mutate(response_touch=ifelse(is.na(date_amt)& !is.na(lead(date_amt)),1,0),
         response_gift=ifelse(lag(response_touch)==1 & !is.na(date_amt), 1,0),
         response_pair=ifelse(response_touch==1 & lead(response_gift)==1 |
                                response_gift==1 & lag(response_touch==1),1,0))%>%
  filter(response_pair==1)%>%
  select(-response_touch, -response_gift)


#Per ID Summary
id_summary<- ira_adj%>%
  ungroup()%>%
  select(-amt, -adj_amt, -ira_law, -ira_gift,
         -gift_num, -pre_law_gift, -post_law_gift,
         -amt_change, -larger_gift, -time_between,
         -time_from_first, -span_quartile)%>%
  group_by(id)%>%
  mutate(num_ira=max(num_ira_gifts))%>%
  filter(row_number(id)==1)

#Year/month summary.Useful for time series conversion and plotting.
#PLEASE NOTE: Amt uses unadjusted amounts due to the fact that we have to view
#IRA gift amounts as nominal amounts.
ira_yearmon<-ira_adj%>%
  ungroup()%>%
  filter(norm_outlier_score<.020)%>%
  filter(decile!="Top" | decile !="10")%>%
  filter(adj_amt<=1000000)%>%
  group_by(yearmon)%>%
  summarize(num_donors=n_distinct(id),
            num_ira_donors=sum(ifelse(row_number(id)==1 & num_ira_gifts>=1,1,0)),
            num_gifts= n(),
            num_ira_gifts=sum(ira_gift),
            num_non_ira_gifts=num_gifts-num_ira_gifts,
            num_first_ira=sum(first_ira),
            num_eligible= sum(yearmon_eligible_flg),
            total_giving=sum(amt),
            total_adj_giving=sum(adj_amt),
            total_adj_non_ira_giving=sum(adj_non_ira_amt),
            total_non_ira_giving=sum(non_ira_amt),
            total_ira_giving=sum(ira_amt),
            mean_amt=mean(amt),
            mean_gift_counts=mean(num_gifts),
            mean_ira_gift_counts=mean(num_ira_gifts),
            mean_non_ira_gift_counts=mean(num_gifts-num_ira_gifts),
            mean_adj_amt=mean(adj_amt),
            mean_ira_amt=mean(ira_amt),
            mean_non_ira_amt=mean(non_ira_amt),
            mean_adj_non_ira_amt=mean(adj_non_ira_amt),
            average_amt=total_giving/num_donors,
            average_adj_amt=total_adj_giving/num_donors,
            median_amt=median(amt),
            median_adj_amt=median(adj_amt),
            max_amt=max(amt),
            max_adj_amt=max(adj_amt),
            avg_time_between=mean(time_between))%>%
  mutate(donor_change=num_donors-lag(num_donors),
         pct_donor_change=(num_donors/lag(num_donors)-1),
         gift_change=num_gifts-lag(num_gifts),
         pct_gift_change=(num_gifts/lag(num_gifts)-1),
         ira_donor_change=num_ira_donors-lag(num_ira_donors),
         ira_gift_change=num_ira_gifts-lag(num_ira_gifts),
         pct_ira_donor_change=(num_ira_donors/lag(num_ira_donors)-1),
         pct_ira_gift_change=(num_ira_gifts/lag(num_ira_gifts)-1),
         num_eligible_change=num_eligible-lag(num_eligible),
         pct_eligible_change=(num_eligible/lag(num_eligible)-1),
         prop_eligible_gift=num_ira_donors/num_eligible,
         prop_ira_donor_change=(num_ira_donors/lag(num_ira_donors)-1),
         prop_ira_donors=num_ira_donors/num_donors,
         prop_ira_gifts=num_ira_gifts/num_gifts,
         prop_ira_gift_change=prop_ira_gifts-lag(prop_ira_gifts),
         total_adj_giving_change=total_adj_giving-lag(total_adj_giving),
         pct_adj_giving_change=(total_adj_giving/lag(total_adj_giving)-1),
         total_adj_ira_giving_change=total_ira_giving-lag(total_ira_giving),
         pct_adj_ira_giving_change=(total_ira_giving/lag(total_ira_giving)-1),
         mean_ira_amt_change=mean_ira_amt-lag(mean_ira_amt),
         pct_ira_mean_change=(mean_ira_amt/lag(mean_ira_amt)-1),
         mean_amt_change=mean_adj_amt-lag(mean_adj_amt),
         pct_mean_change=(mean_adj_amt/lag(mean_adj_amt)-1))

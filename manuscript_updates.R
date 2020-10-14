load("all.RData")
load("genericPIV.RData")
load("genericnoPIV.RData")
load("NDC_noPIV.Rdata")
load("NDC_PIV.Rdata")



###manuscript updates
#1. By year analysis
## With PC
genericPIV_origin <- genericPIV_origin %>%
  mutate(index_year = year(exclusivity),
         lag = as.numeric(lag))

genericPIV <- genericPIV %>%
  mutate(index_year = year(exclusivity),
         lag = as.numeric(lag)) 


mean_lag_by_order <- function(df, entrant){
  df_entrant <- df %>%
    filter(order == entrant) 
  
  df_entrant_lag <- df_entrant %>%
    group_by(index_year) %>%
    summarise(mean_time = mean(lag)) 
  
  return(df_entrant_lag)
}

median_lag_by_order <- function(df, entrant){
  df_entrant <- df %>%
    filter(order == entrant) 
  
  df_entrant_lag <- df_entrant %>%
    group_by(index_year) %>%
    summarise(median_time = median(lag)) 
  
  return(df_entrant_lag)
}

top10_mean_lag_by_order <- function(df, entrant){
  df_entrant <- df %>%
    filter(order == entrant) 
  
  df_entrant_lag <- df_entrant %>%
    arrange(index_year, lag) %>%
    group_by(index_year) %>% 
    top_n(n = -10, wt = lag) %>%
    summarise(mean_time = mean(lag)) 
  
  return(df_entrant_lag)
}

PC_second_mean_lag <- mean_lag_by_order(df = genericPIV_origin, entrant = 2)

PC_third_mean_lag <- mean_lag_by_order(df = genericPIV_origin, entrant = 3)

PC_second_median_lag <- median_lag_by_order(df = genericPIV_origin, entrant = 2)

PC_third_median_lag <- median_lag_by_order(df = genericPIV_origin, entrant = 3)

ggplot(data = PC_second_mean_lag, aes(y = mean_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = PC_second_median_lag, aes(y = median_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = PC_third_mean_lag, aes(y = mean_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = PC_third_median_lag, aes(y = median_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 


#apply 5-year censoring window to dataset
#here exclude if entry2 == 0
genericPIV_exclude_censor <- genericPIV %>%
  filter(entry2 == 1)

PC_second_mean_lag_exclude_censor <- mean_lag_by_order(df = genericPIV_exclude_censor, entrant = 2)

PC_third_mean_lag_exclude_censor <- mean_lag_by_order(df = genericPIV_exclude_censor, entrant = 3)

PC_second_median_lag_exclude_censor <- median_lag_by_order(df = genericPIV_exclude_censor, entrant = 2)

PC_third_median_lag_exclude_censor <- median_lag_by_order(df = genericPIV_exclude_censor, entrant = 3)


ggplot(data = PC_second_mean_lag_exclude_censor, aes(y = mean_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = PC_second_median_lag_exclude_censor, aes(y = median_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = PC_third_mean_lag_exclude_censor, aes(y = mean_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = PC_third_median_lag_exclude_censor, aes(y = median_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 


PC_second_mean_lag_top10 <- top10_mean_lag_by_order(df = genericPIV_exclude_censor, entrant = 2)

PC_third_mean_lag_top10 <- top10_mean_lag_by_order(df = genericPIV_exclude_censor, entrant = 3)

ggplot(data = PC_second_mean_lag_top10, aes(y = mean_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = PC_third_mean_lag_top10, aes(y = mean_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

######################################################################################
## Without PC
genericnoPIV_origin <- genericnoPIV_origin %>%
  mutate(index_year = year(min),
         lag = t1)

genericnoPIV <- genericnoPIV %>%
  mutate(index_year = year(min),
         lag = t1) 


noPC_second_mean_lag <- mean_lag_by_order(df = genericnoPIV_origin, entrant = 2)

noPC_third_mean_lag <- mean_lag_by_order(df = genericnoPIV_origin, entrant = 3)

noPC_second_median_lag <- median_lag_by_order(df = genericnoPIV_origin, entrant = 2)

noPC_third_median_lag <- median_lag_by_order(df = genericnoPIV_origin, entrant = 3)

ggplot(data = noPC_second_mean_lag, aes(y = mean_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = noPC_second_median_lag, aes(y = median_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = noPC_third_mean_lag, aes(y = mean_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = noPC_third_median_lag, aes(y = median_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 


#apply 5-year censoring window to dataset
#here exclude if entry2 == 0
genericnoPIV_exclude_censor <- genericnoPIV %>%
  filter(entry1 == 1)

noPC_first_mean_lag_exclude_censor <- mean_lag_by_order(df = genericnoPIV_exclude_censor, entrant = 1)

noPC_second_mean_lag_exclude_censor <- mean_lag_by_order(df = genericnoPIV_exclude_censor, entrant = 2)

noPC_third_mean_lag_exclude_censor <- mean_lag_by_order(df = genericnoPIV_exclude_censor, entrant = 3)

noPC_first_median_lag_exclude_censor <- median_lag_by_order(df = genericnoPIV_exclude_censor, entrant = 1)

noPC_second_median_lag_exclude_censor <- median_lag_by_order(df = genericnoPIV_exclude_censor, entrant = 2)

noPC_third_median_lag_exclude_censor <- median_lag_by_order(df = genericnoPIV_exclude_censor, entrant = 3)

ggplot(data = noPC_first_mean_lag_exclude_censor, aes(y = mean_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = noPC_first_median_lag_exclude_censor, aes(y = median_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = noPC_second_mean_lag_exclude_censor, aes(y = mean_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = noPC_second_median_lag_exclude_censor, aes(y = median_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = noPC_third_mean_lag_exclude_censor, aes(y = mean_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = noPC_third_median_lag_exclude_censor, aes(y = median_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 




noPC_first_mean_lag_top10 <- top10_mean_lag_by_order(df = genericnoPIV_exclude_censor, entrant = 1)

noPC_second_mean_lag_top10 <- top10_mean_lag_by_order(df = genericnoPIV_exclude_censor, entrant = 2)

noPC_third_mean_lag_top10 <- top10_mean_lag_by_order(df = genericnoPIV_exclude_censor, entrant = 3)

ggplot(data = noPC_first_mean_lag_top10, aes(y = mean_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = noPC_second_mean_lag_top10, aes(y = mean_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 

ggplot(data = noPC_third_mean_lag_top10, aes(y = mean_time, x = index_year)) + 
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(2007,2017,1)) 


## 2. Check approval date vs. marketing date 
NDC_noPIV <- NDC_noPIV %>%
  dplyr::select(index, STARTMARKETINGDATE, Approval_Date, year_LOE) %>%
  mutate(marketing_date = as.Date(STARTMARKETINGDATE, "%Y%m%d"),
         approval_date = as.Date(Approval_Date, "%B %d, %Y"),
         market_delay = marketing_date - approval_date)

median(NDC_noPIV$market_delay)

NDC_PIV <- NDC_PIV %>%
  dplyr::select(index, STARTMARKETINGDATE, Approval_Date, year_LOE) %>%
  mutate(marketing_date = as.Date(STARTMARKETINGDATE, "%Y%m%d"),
         approval_date = as.Date(Approval_Date, "%B %d, %Y"),
         market_delay = marketing_date - approval_date)

median(NDC_PIV$market_delay)

## 3. Apply 2-year censoring window 
#add 2-year administrative censoring for each year
## PC
genericPIV_0708 <- genericPIV_origin %>%
  filter(exclusivity < '2009-10-01' & date < '2009-10-01')

genericPIV_0708 <- add_censored(genericPIV_0708) %>%
  mutate(year = "07/08")

genericPIV_0708_mean <- genericPIV_0708 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2008)

genericPIV_0809 <- genericPIV_origin %>%
  filter(exclusivity >= '2008-10-01' & date >= '2008-10-01',
    exclusivity < '2010-10-01' & date < '2010-10-01'
         )

genericPIV_0809 <- add_censored(genericPIV_0809) %>%
  mutate(year = "08/09")

genericPIV_0809_mean <- genericPIV_0809 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2009)

genericPIV_0910 <- genericPIV_origin %>%
  filter(exclusivity >= '2009-10-01' & date >= '2009-10-01',
         exclusivity < '2011-10-01' & date < '2011-10-01'
  )

genericPIV_0910 <- add_censored(genericPIV_0910) %>%
  mutate(year = "09/10")

genericPIV_0910_mean <- genericPIV_0910 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2010)

genericPIV_1011 <- genericPIV_origin %>%
  filter(exclusivity >= '2010-10-01' & date >= '2010-10-01',
         exclusivity < '2012-10-01' & date < '2012-10-01'
  )

genericPIV_1011 <- add_censored(genericPIV_1011) %>%
  mutate(year = "10/11")

genericPIV_1011_mean <- genericPIV_1011 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2011)

genericPIV_1112 <- genericPIV_origin %>%
  filter(exclusivity >= '2011-10-01' & date >= '2011-10-01',
         exclusivity < '2013-10-01' & date < '2013-10-01'
  )

genericPIV_1112 <- add_censored(genericPIV_1112) %>%
  mutate(year = "11/12")

genericPIV_1112_mean <- genericPIV_1112 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2012)

genericPIV_1213 <- genericPIV_origin %>%
  filter(exclusivity >= '2012-10-01' & date >= '2012-10-01',
         exclusivity < '2014-10-01' & date < '2014-10-01'
  )

genericPIV_1213 <- add_censored(genericPIV_1213) %>%
  mutate(year = "12/13")

genericPIV_1213_mean <- genericPIV_1213 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2013)

genericPIV_1314 <- genericPIV_origin %>%
  filter(exclusivity >= '2013-10-01' & date >= '2013-10-01',
         exclusivity < '2015-10-01' & date < '2015-10-01'
  )

genericPIV_1314 <- add_censored(genericPIV_1314) %>%
  mutate(year = "13/14")

genericPIV_1314_mean <- genericPIV_1314 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2014)

genericPIV_1415 <- genericPIV_origin %>%
  filter(exclusivity >= '2014-10-01' & date >= '2014-10-01',
         exclusivity < '2016-10-01' & date < '2016-10-01'
  )

genericPIV_1415 <- add_censored(genericPIV_1415) %>%
  mutate(year = "14/15")

genericPIV_1415_mean <- genericPIV_1415 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2015)

genericPIV_1516 <- genericPIV_origin %>%
  filter(exclusivity >= '2015-10-01' & date >= '2015-10-01',
         exclusivity < '2017-10-01' & date < '2017-10-01'
  )

genericPIV_1516 <- add_censored(genericPIV_1516) %>%
  mutate(year = "15/16")

genericPIV_1516_mean <- genericPIV_1516 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2016)

#genericPIV_1617 <- genericPIV_origin %>%
#  filter(exclusivity >= '2016-10-01' & date >= '2016-10-01',
#         exclusivity < '2017-10-01' & date < '2017-10-01'
#  )

#genericPIV_1617_mean <- genericPIV_1617 %>%
#  group_by(order) %>%
#  summarise(mean = mean(as.numeric(lag))) %>%
#  mutate(year = 2017)

genericPIV_mean <- genericPIV_0708_mean %>%
  bind_rows(genericPIV_0809_mean) %>%
  bind_rows(genericPIV_0910_mean) %>%  
  bind_rows(genericPIV_1011_mean) %>%  
  bind_rows(genericPIV_1112_mean) %>%  
  bind_rows(genericPIV_1213_mean) %>%  
  bind_rows(genericPIV_1314_mean) %>%  
  bind_rows(genericPIV_1415_mean) %>%  
  bind_rows(genericPIV_1516_mean) #%>%  
  #bind_rows(genericPIV_1617_mean) 

genericPIV_all <- genericPIV_0708 %>%
  bind_rows(genericPIV_0809) %>%
  bind_rows(genericPIV_0910) %>%  
  bind_rows(genericPIV_1011) %>%  
  bind_rows(genericPIV_1112) %>%  
  bind_rows(genericPIV_1213) %>%  
  bind_rows(genericPIV_1314) %>%  
  bind_rows(genericPIV_1415) %>%  
  bind_rows(genericPIV_1516) 

genericPIV_mean <- genericPIV_mean %>%
  filter(order != 1 & order <= 6)

genericPIV_mean_plot <- ggplot(data=genericPIV_mean, aes(x=as.character(year), y=mean, group = as.factor(order), colour = as.factor(order))) +
  geom_line() +
  geom_point() +
  ggtitle("Mean time to entry for different order of entrants") +
  xlab("Year") +
  ylab("mean time to entry") 

genericPIV_mean_plot

plot_KM(genericPIV_all, entrant = 2)
plot_KM(genericPIV_all, entrant = 3)
plot_KM(genericPIV_all, entrant = 4)

genericPIV_postGDUFA <- genericPIV_all %>%
  filter(year %in% c("12/13", "13/14", "14/15", "15/16"))

plot_KM(genericPIV_postGDUFA, entrant = 2)


## No PC
genericnoPIV_origin$lag <- as.Date(genericnoPIV_origin$date)-as.Date(genericnoPIV_origin$min)

genericnoPIV_negative_index <- genericnoPIV_origin %>%
  filter(as.numeric(lag) < 0) %>%
  distinct(index)

genericnoPIV_origin <- genericnoPIV_origin %>%
  anti_join(genericnoPIV_negative_index, by = "index")

genericnoPIV_0708 <- genericnoPIV_origin %>%
  filter(min < '2009-10-01' & date < '2009-10-01')

add_censored <- function(data){
  data_index <- data %>% 
    group_by(index) %>% 
    summarise(count_generic=n()) %>% 
    as.data.frame()
  
  data <- data %>%
    left_join(data_index) %>%
    mutate(censor = 0)
  
  data_censored <- data %>%
    filter(order == count_generic) %>%
    mutate(order = order + 1,
           censor = 1) 
  
  data <- data %>%
    bind_rows(data_censored) 
  
  return(data)
}

genericnoPIV_0708 <- add_censored(genericnoPIV_0708) %>%
  mutate(year = "07/08")

#genericnoPIV_0708$SurvObj <- with(genericnoPIV_0708, Surv(lag, censor))
#km_0708 <- survfit(SurvObj ~ 1, data = genericnoPIV_0708, conf.type = "log-log")
#plot(km_0708)

genericnoPIV_0708_mean <- genericnoPIV_0708 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2008)

genericnoPIV_0809 <- genericnoPIV_origin %>%
  filter(min >= '2008-10-01' & date >= '2008-10-01',
         min < '2010-10-01' & date < '2010-10-01'
  )

genericnoPIV_0809 <- add_censored(genericnoPIV_0809) %>%
  mutate(year = "08/09")

genericnoPIV_0809_mean <- genericnoPIV_0809 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2009)

genericnoPIV_0910 <- genericnoPIV_origin %>%
  filter(min >= '2009-10-01' & date >= '2009-10-01',
         min < '2011-10-01' & date < '2011-10-01'
  )

genericnoPIV_0910 <- add_censored(genericnoPIV_0910) %>%
  mutate(year = "09/10")

genericnoPIV_0910_mean <- genericnoPIV_0910 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2010)

genericnoPIV_1011 <- genericnoPIV_origin %>%
  filter(min >= '2010-10-01' & date >= '2010-10-01',
         min < '2012-10-01' & date < '2012-10-01'
  )

genericnoPIV_1011 <- add_censored(genericnoPIV_1011) %>%
  mutate(year = "10/11")

genericnoPIV_1011_mean <- genericnoPIV_1011 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2011)

genericnoPIV_1112 <- genericnoPIV_origin %>%
  filter(min >= '2011-10-01' & date >= '2011-10-01',
         min < '2013-10-01' & date < '2013-10-01'
  )

genericnoPIV_1112 <- add_censored(genericnoPIV_1112) %>%
  mutate(year = "11/12")

genericnoPIV_1112_mean <- genericnoPIV_1112 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2012)

genericnoPIV_1213 <- genericnoPIV_origin %>%
  filter(min >= '2012-10-01' & date >= '2012-10-01',
         min < '2014-10-01' & date < '2014-10-01'
  )

genericnoPIV_1213 <- add_censored(genericnoPIV_1213) %>%
  mutate(year = "12/13")


genericnoPIV_1213_mean <- genericnoPIV_1213 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2013)

genericnoPIV_1314 <- genericnoPIV_origin %>%
  filter(min >= '2013-10-01' & date >= '2013-10-01',
         min < '2015-10-01' & date < '2015-10-01'
  )

genericnoPIV_1314 <- add_censored(genericnoPIV_1314) %>%
  mutate(year = "13/14")

genericnoPIV_1314_mean <- genericnoPIV_1314 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2014)

genericnoPIV_1415 <- genericnoPIV_origin %>%
  filter(min >= '2014-10-01' & date >= '2014-10-01',
         min < '2016-10-01' & date < '2016-10-01'
  )

genericnoPIV_1415 <- add_censored(genericnoPIV_1415) %>%
  mutate(year = "14/15")

genericnoPIV_1415_mean <- genericnoPIV_1415 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2015)

genericnoPIV_1516 <- genericnoPIV_origin %>%
  filter(min >= '2015-10-01' & date >= '2015-10-01',
         min < '2017-10-01' & date < '2017-10-01'
  )

genericnoPIV_1516 <- add_censored(genericnoPIV_1516) %>%
  mutate(year = "15/16")

genericnoPIV_1516_mean <- genericnoPIV_1516 %>%
  group_by(order) %>%
  summarise(mean = mean(as.numeric(lag))) %>%
  mutate(year = 2016)

#genericnoPIV_1617 <- genericnoPIV_origin %>%
#  filter(min >= '2016-10-01' & date >= '2016-10-01',
#         min < '2017-10-01' & date < '2017-10-01'
#  )

#genericnoPIV_1617_mean <- genericnoPIV_1617 %>%
#  group_by(order) %>%
#  summarise(mean = mean(as.numeric(lag))) %>%
#  mutate(year = 2017)

genericnoPIV_mean <- genericnoPIV_0708_mean %>%
  bind_rows(genericnoPIV_0809_mean) %>%
  bind_rows(genericnoPIV_0910_mean) %>%  
  bind_rows(genericnoPIV_1011_mean) %>%  
  bind_rows(genericnoPIV_1112_mean) %>%  
  bind_rows(genericnoPIV_1213_mean) %>%  
  bind_rows(genericnoPIV_1314_mean) %>%  
  bind_rows(genericnoPIV_1415_mean) %>%  
  bind_rows(genericnoPIV_1516_mean) #%>%  
  #bind_rows(genericnoPIV_1617_mean) 

genericnoPIV_all <- genericnoPIV_0708 %>%
  bind_rows(genericnoPIV_0809) %>%
  bind_rows(genericnoPIV_0910) %>%  
  bind_rows(genericnoPIV_1011) %>%  
  bind_rows(genericnoPIV_1112) %>%  
  bind_rows(genericnoPIV_1213) %>%  
  bind_rows(genericnoPIV_1314) %>%  
  bind_rows(genericnoPIV_1415) %>%  
  bind_rows(genericnoPIV_1516) 

genericnoPIV_mean <- genericnoPIV_mean %>%
  filter(order <= 6)

genericnoPIV_mean_plot <- ggplot(data=genericnoPIV_mean, aes(x=as.character(year), y=mean, group = as.factor(order), colour = as.factor(order))) +
  geom_line() +
  geom_point() +
  ggtitle("Mean time to entry for different order of entrants") +
  xlab("Year") +
  ylab("mean time to entry") 

genericnoPIV_mean_plot

genericnoPIV_second <- genericnoPIV_all %>%
  filter(order == 2)
km_noPIV_second <- survfit(Surv(lag, censor) ~ year, data = genericnoPIV_second)
ggsurvplot(km_noPIV_second, data = genericnoPIV_second)

plot_KM <- function(data, entrant){
  data <- data %>%
    filter(order == entrant)
  km <- survfit(Surv(lag, censor) ~ year, data = data)
  plot <- ggsurvplot(km, data = data)
  return(plot)
}

genericnoPIV_postGDUFA <- genericnoPIV_all %>%
  filter(year %in% c("12/13", "13/14", "14/15", "15/16"))

#plot_KM(genericnoPIV_all, entrant = 1)
plot_KM(genericnoPIV_all, entrant = 2)
plot_KM(genericnoPIV_all, entrant = 3)
plot_KM(genericnoPIV_all, entrant = 4)

plot_KM(genericnoPIV_postGDUFA, entrant = 2)
plot_KM(genericnoPIV_postGDUFA, entrant = 3)
plot_KM(genericnoPIV_postGDUFA, entrant = 4)

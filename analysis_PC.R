###PIV#################################################################
##For each index, keep if PIV=1 
##This is the dataset with generic entry related to first filer entering with PIV
genericPIV<-generic %>% group_by(index) %>% filter(any(PIV == 1))
#Apply exclusivity end date to everyone within group
#Calculate time difference
require(tidyverse)
genericPIV <- genericPIV %>% group_by(index) %>% fill(exclusivity) %>% fill(exclusivity, .direction = "up")
genericPIV$lag<-as.Date(genericPIV$date)-as.Date(genericPIV$exclusivity)
genericPIV$lag[genericPIV$PIV==1]<-0
#Delete those with negative lag time???
genericPIV<-genericPIV[genericPIV[, "lag"]>=0, ]

#add restrict for index date >= 20071001
genericPIV <- genericPIV %>%
  filter(exclusivity >= "2007-10-01")

genericPIV_origin <- genericPIV

##add administrative censoring for pre-GDUFA index dates
#for index date pre-GDUFA, exclude generics with entry date post-GDUFA
genericPIV_preGDUFA <- genericPIV %>%
  filter(exclusivity < '2012-10-01' & date < '2012-10-01')

#Here, missing cases when exclusivity < '2012-10-01' and second entrant's date >='2012-10-01'; need to add branded drug as censored

genericPIV <- genericPIV %>%
  filter(exclusivity >= '2012-10-01') %>%
  bind_rows(genericPIV_preGDUFA)

#Update order
genericPIV <- genericPIV %>%
  group_by(index) %>%
  mutate(order = rank(numdate,ties.method="first"))


#Rank by time of approval for the order of entry within group: entry
#Drugs entering at the same time would receive the same ranking/entry
#genericPIV <- genericPIV %>% arrange(index, date) %>% group_by(index) %>%
#  ungroup()  %>% as.data.frame()

#genericPIV <- genericPIV %>%
#  group_by(index) %>%
#  mutate(entry = rank(numdate,ties.method="min"))
#If different rank for drugs entering at the same time
#genericPIV$order <- sequence(tabulate(genericPIV$index)) 
#genericPIV <- genericPIV %>%
#  group_by(index) %>%
#  mutate(order = rank(numdate,ties.method="first"))

#Constructing variable for more than one entry:
genericPIV$entry2 <- 1
genericPIV$entry2[genericPIV$order == 1] <- 0

#construct variables for start time and stop time
genericPIV <- genericPIV %>% arrange(index, order) 
genericPIV <- genericPIV %>% group_by(index) %>% mutate(t0 = lag(lag), t1 = lag, gaptime = lag - lag(lag))



#count total generic again - exclude generics with only first entrant
genericPIV_index <- genericPIV %>% group_by(index) %>% summarise(count_generic=n()) %>% as.data.frame()

genericPIV <- left_join(genericPIV, genericPIV_index)

genericPIV$GDUFA <- 0
genericPIV$GDUFA[genericPIV$numdate >= 20121001] <- 1
genericPIV$indexyear <- round(as.integer(format(as.Date(genericPIV$exclusivity), "%Y%m%d"))/10000)


genericPIV_nocensor <- genericPIV

genericPIV_first <- genericPIV %>% filter(order == 1)

genericPIV<- genericPIV %>% filter(order != 1)

#genericPIV$count_generic[genericPIV$count_generic >= 6] <- 6


#censored - first entrants with no second entrants yet
genericPIV_first_only <- genericPIV_first %>% filter(count_generic == 1) 

#for these ones, use start time = 0, calculate stop time as "2017-09-31 - approval date"
genericPIV_first_only$t0 <- 0

genericPIV_first_only_preGDUFA <- genericPIV_first_only %>%
  filter(exclusivity < '2012-10-01') 

genericPIV_first_only_postGDUFA <- genericPIV_first_only %>%
  filter(exclusivity >= '2012-10-01' & date >= '2012-10-01' ) 

genericPIV_first_only_preGDUFA$gaptime <- genericPIV_first_only_preGDUFA$t1 <- as.numeric(as.Date("2012-09-30") - as.Date(genericPIV_first_only_preGDUFA$date))
genericPIV_first_only_postGDUFA$gaptime <- genericPIV_first_only_postGDUFA$t1 <- as.numeric(as.Date("2017-09-30") - as.Date(genericPIV_first_only_postGDUFA$date))

genericPIV_first_only <- genericPIV_first_only_preGDUFA %>%
  bind_rows(genericPIV_first_only_postGDUFA)

genericPIV_first_only$GDUFA[genericPIV_first_only$date >= "2012-10-01" ] <- 1

#also add first entrants with exclusivity <= 2012-10-01, but have second generic entry after this
genericPIV_first_preGDUFA <- genericPIV_origin %>%
  filter(exclusivity < '2012-10-01' & date >= '2012-10-01' & order == 2)
genericPIV_first_preGDUFA$entry1 <- 0
genericPIV_first_preGDUFA$t0 <- 0
genericPIV_first_preGDUFA$gaptime <- genericPIV_first_preGDUFA$t1 <- as.numeric(as.Date("2012-09-30") - as.Date(genericPIV_first_preGDUFA$exclusivity))
genericPIV_first_only <- genericPIV_first_only %>%
  bind_rows(genericPIV_first_preGDUFA)

#genericPIV_first_only$t1[genericPIV_first_only$exclusivity >= "2012-10-01"] <- as.numeric(as.Date("2017-09-30") - as.Date(genericPIV_first_only$date[genericPIV_first_only$exclusivity >= "2012-10-01"]))
#genericPIV_first_only$t1[genericPIV_first_only$exclusivity < "2012-10-01"] <- as.numeric(as.Date("2012-09-30") - as.Date(genericPIV_first_only$date[genericPIV_first_only$exclusivity < "2012-10-01"]))
#genericPIV_first_only$gaptime[genericPIV_first_only$exclusivity >= "2012-10-01"] <- as.numeric(as.Date("2017-09-30") - as.Date(genericPIV_first_only$date[genericPIV_first_only$exclusivity >= "2012-10-01"]))
#genericPIV_first_only$gaptime[genericPIV_first_only$exclusivity < "2012-10-01"] <- as.numeric(as.Date("2012-09-30") - as.Date(genericPIV_first_only$date[genericPIV_first_only$exclusivity < "2012-10-01"]))


genericPIV$t0 <- as.numeric(genericPIV$t0)
genericPIV$t1 <- as.numeric(genericPIV$t1)
genericPIV$gaptime <- as.numeric(genericPIV$gaptime)

genericPIV$GDUFA <- 0
genericPIV$GDUFA[genericPIV$numdate >= 20121001] <- 1

table(genericPIV$ATC1, genericPIV$GDUFA)
ATC_PIV <- data.frame(table(genericPIV$ATC1, genericPIV$GDUFA))
colnames(ATC_PIV)[1] <- "ATC1"
colnames(ATC_PIV)[2] <- "GDUFA"
colnames(ATC_PIV)[3] <- "Number"


genericPIV <- rbind(genericPIV, genericPIV_first_only)

genericPIV$t0[is.na(genericPIV$t0)] <- 0

genericPIV$GDUFA <- 0
genericPIV$GDUFA[genericPIV$numdate >= 20121001] <- 1

#Recode order of entry
genericPIV$ncompetitor <- genericPIV$order - 1
genericPIV$ncompetitor[genericPIV$order == 1] <- 1

#Generate index year (exclusivity year)
genericPIV$indexyear <- round(as.integer(format(as.Date(genericPIV$exclusivity), "%Y%m%d"))/10000)

#Make year 2012 the reference year
genericPIV$indexyear_cat <- as.factor(genericPIV$indexyear)
genericPIV$indexyear_cat <- relevel(genericPIV$indexyear_cat, ref = "2012")
genericPIV$indexyear <- genericPIV$indexyear - 2012

#Add censored information
#use different admin censoring date for pre and post GDUFA 
genericPIV_censor <- genericPIV %>% filter(order == count_generic)
genericPIV_censor <- genericPIV_censor %>% filter(entry2 == 1)
genericPIV_censor$entry2 <- 0
genericPIV_censor$ncompetitor <- genericPIV_censor$ncompetitor + 1
genericPIV_censor$t0 <- genericPIV_censor$t1

genericPIV_censor_preGDUFA <- genericPIV_censor %>%
  filter(exclusivity < "2012-10-01")
genericPIV_censor_postGDUFA <- genericPIV_censor %>%
  filter(exclusivity >= "2012-10-01")

genericPIV_censor_preGDUFA$t1 <- as.numeric(as.Date("2012-09-30") - as.Date(genericPIV_censor_preGDUFA$exclusivity))
genericPIV_censor_preGDUFA$t1 <- as.numeric(as.Date("2017-09-30") - as.Date(genericPIV_censor_postGDUFA$exclusivity))

genericPIV_censor <- genericPIV_censor_preGDUFA %>%
  bind_rows(genericPIV_censor_postGDUFA)

#genericPIV_censor$t1[genericPIV_censor$exclusivity >= "2012-10-01"] <- as.numeric(as.Date("2017-09-30") - as.Date(genericPIV_censor$exclusivity[genericPIV_censor$exclusivity >= "2012-10-01"]))
#genericPIV_censor$t1[genericPIV_censor$exclusivity < "2012-10-01"] <- as.numeric(as.Date("2012-09-30") - as.Date(genericPIV_censor$exclusivity[genericPIV_censor$exclusivity < "2012-10-01"]))
genericPIV_censor$gaptime <- as.numeric(as.Date(genericPIV_censor$t1) - as.Date(genericPIV_censor$t0))

#Change folow-up time for censored cases before GDUFA.




#merge back to main dataset
genericPIV <- rbind(genericPIV, genericPIV_censor)

#Keep k <= 6
genericPIV <- genericPIV %>% filter(order <= 6)
genericPIV <- genericPIV %>% filter(ncompetitor <= 5)



#genericPIV$indexyear <- as.numeric(genericPIV$indexyear) - 2012


#Start time = stop time will be excluded, try:
genericPIV$t1[genericPIV$t0 == genericPIV$t1] <- as.numeric(genericPIV$t1[genericPIV$t0 == genericPIV$t1]) + 0.001

model_PIV_test_unadj <- coxph(Surv(t0, t1, entry2) ~ GDUFA*as.factor(ncompetitor) + cluster(index), method = "breslow", data = genericPIV)
summary(model_PIV_test_unadj) 

model_PIV_test_adj <- coxph(Surv(t0, t1, entry2) ~ GDUFA*as.factor(ncompetitor) + route + AG + Tier + ATC1 + ETASU + guidance_before + indexyear + cluster(index), method = "breslow", data = genericPIV)
summary(model_PIV_test_adj) 

#Stratified PWP-TT
model_PIV_PWPTT_unadj <- coxph(Surv(t0, t1, entry2) ~ GDUFA*strata(ncompetitor) + cluster(index), method = "breslow", data = genericPIV)
summary(model_PIV_PWPTT_unadj) 

model_PIV_PWPTT_adj <- coxph(Surv(t0, t1, entry2) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV  + cluster(index), method = "breslow", data = genericPIV)
#model_PIV_PWPTT_adj <- coxph(Surv(t0, t1, entry2) ~ (GDUFA+ route + AG + ATC1 + ETASU + guidance_before + indexyear)*strata(ncompetitor) + cluster(index), method = "breslow", data = genericPIV)

summary(model_PIV_PWPTT_adj)

#Stratified PWP-GT
genericPIV$gaptime_start <- 0
genericPIV$gaptime[genericPIV$gaptime == 0] <- as.numeric(genericPIV$gaptime[genericPIV$gaptime == 0]) + 0.001

model_PIV_PWPGT_unadj <- coxph(Surv(genericPIV$gaptime_start, genericPIV$gaptime, entry2) ~ GDUFA*strata(ncompetitor) + cluster(index), method = "breslow", data = genericPIV)
summary(model_PIV_PWPGT_unadj)

#ggforest(model_PIV_PWPGT_unadj, data = genericPIV)

model_PIV_PWPGT_adj <- coxph(Surv(genericPIV$gaptime_start, genericPIV$gaptime, entry2) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericPIV)
#model_PIV_PWPGT_adj <- coxph(Surv(genericPIV$gaptime_start, genericPIV$gaptime, entry2) ~ (GDUFA + route + AG + ATC1 + ETASU + guidance_before + indexyear)*strata(ncompetitor)  + cluster(index), method = "breslow", data = genericPIV)

summary(model_PIV_PWPGT_adj)

#Frailty
model_PIV_frailty_unadj <- coxph(Surv(genericPIV$gaptime_start, genericPIV$gaptime, entry2) ~ GDUFA + as.factor(ncompetitor) + frailty.gamma(index, eps=1e-10, method="aic", sparse=0),
                                 outer.max=1000, iter.max=10000, data = genericPIV)
model_PIV_frailty_unadj <- coxph(Surv(genericPIV$gaptime_start, genericPIV$gaptime, entry2) ~ GDUFA*strata(ncompetitor) + frailty.gamma(index, eps=1e-10, method="aic", sparse=0),
                                 outer.max=1000, iter.max=10000, data = genericPIV)
model_PIV_frailty_unadj <- coxph(Surv(genericPIV$gaptime_start, genericPIV$gaptime, entry2) ~ GDUFA*strata(ncompetitor) + frailty(index), data = genericPIV)

summary(model_PIV_frailty_unadj)

#model_PIV_frailty_adj <- coxph(Surv(t1, entry2) ~ GDUFA + as.factor(ncompetitor) + route + AG + Tier + ATC1 + ETASU + guidance_before + indexyear + frailty.gamma(index, eps=1e-10, method="aic", sparse=0),
#                           outer.max=1000, iter.max=10000, data = genericPIV)
#summary(model_PIV_frailty_adj)

model_PIV_frailty_TT_adj <- coxph(Surv(t0, t1, entry2) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + frailty(index), data = genericPIV)

summary(model_PIV_frailty_TT_adj)


model_PIV_frailty_GT_adj <- coxph(Surv(genericPIV$gaptime_start, genericPIV$gaptime, entry2) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + frailty(index), data = genericPIV)

summary(model_PIV_frailty_GT_adj)


#AG
model_PIV_AG_unadj <- coxph(Surv(t0, t1, entry2) ~ GDUFA, method = "breslow", robust = T, data = genericPIV)
summary(model_PIV_AG_unadj) 

model_PIV_AG_adj <- coxph(Surv(t0, t1, entry2) ~ GDUFA + route + AG + ATC1 + ETASU + guidance_before + indexyear, method = "breslow", robust = T, data = genericPIV)
summary(model_PIV_AG_adj)

##Exclude index date 6-month before GDUFA
genericPIV_6mon <- genericPIV %>% filter(exclusivity < "2012-04-01" | exclusivity >= "2012-10-01" )
##PWP-GT
model_PIV_6mon_PWPGT_adj <- coxph(Surv(gaptime_start, gaptime, entry2) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before + indexyear +  ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericPIV_6mon)
summary(model_PIV_6mon_PWPGT_adj)
##PWP-TT
model_PIV_6mon_PWPTT_adj <- coxph(Surv(t0, t1, entry2) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before  + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericPIV_6mon)
summary(model_PIV_6mon_PWPTT_adj)

###Limit to 1500 days
genericPIV_1500 <- genericPIV %>% filter(lag <= 1500)
##PWP-GT
model_PIV_1500_PWPGT_adj <- coxph(Surv(genericPIV_1500$gaptime_start, genericPIV_1500$gaptime, entry2) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericPIV_1500)
summary(model_PIV_1500_PWPGT_adj)
##PWP-TT
model_PIV_1500_PWPTT_adj <- coxph(Surv(t0, t1, entry2) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before  + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericPIV_1500)
summary(model_PIV_1500_PWPTT_adj)

##Add seasonality 
genericPIV$month <- genericPIV$approvemonth%%100

model_PIV_month_PWPGT_adj <- coxph(Surv(genericPIV$gaptime_start, genericPIV$gaptime, entry2) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + as.factor(month) + cluster(index), method = "breslow", data = genericPIV)
summary(model_PIV_month_PWPGT_adj)

model_PIV_month_PWPTT_adj <- coxph(Surv(t0, t1, entry2) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before + indexyear+ ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV  + as.factor(month) + cluster(index), method = "breslow", data = genericPIV)
summary(model_PIV_month_PWPTT_adj)

##Control for ACA
genericPIV <- genericPIV %>% mutate(ACA = ifelse(numdate >= 20140101, 1, 0))

model_PIV_PWPTT_ACA_adj <- coxph(Surv(t0, t1, entry2) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before + indexyear + ACA + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV  + cluster(index), method = "breslow", data = genericPIV)
summary(model_PIV_PWPTT_ACA_adj)

model_PIV_PWPGT_ACA_adj <- coxph(Surv(genericPIV$gaptime_start, genericPIV$gaptime, entry2) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before + indexyear + ACA + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericPIV)
summary(model_PIV_PWPGT_ACA_adj)

##Subgroup of drugs with multiple PCs
genericPIV <- genericPIV %>%
  mutate(PIV = ifelse(is.na(PIV) | PIV == 0, 0, 1))

genericPIV_multi_index <- genericPIV %>%
  group_by(index) %>%
  dplyr::summarise(PIV_n = sum(PIV))

genericPIV <- genericPIV %>%
  left_join(genericPIV_multi_index, by = "index")

genericPIV_multi_PC <- genericPIV %>%
  ungroup %>% 
  filter(PIV_n != 0) 

model_PIV_PWPTT_multi_adj <- coxph(Surv(t0, t1, entry2) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV  + cluster(index), method = "breslow", data = genericPIV_multi_PC)
summary(model_PIV_PWPTT_multi_adj)

model_PIV_month_PWPGT_multi_adj <- coxph(Surv(genericPIV_multi_PC$gaptime_start, genericPIV_multi_PC$gaptime, entry2) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + as.factor(month) + cluster(index), method = "breslow", data = genericPIV_multi_PC)
summary(model_PIV_month_PWPGT_multi_adj)


##After GDUFA, PWP-GT
genericPIV_postGDUFA <- genericPIV %>% filter(exclusivity >= "2012-10-01")

model_PIV_PWPGT_post <- coxph(Surv(genericPIV_postGDUFA$gaptime_start, genericPIV_postGDUFA$gaptime, entry2) ~ strata(ncompetitor) + route + AG + ATC1 + ETASU + guidance_before + indexyear + cluster(index), method = "breslow", data = genericPIV_postGDUFA)
summary(model_PIV_PWPGT_post)

h_test <- basehaz(model_PIV_PWPGT_post, centered = T)
h_test <- h_test %>% mutate(h2 = hazard*2)
h_test1 <- h_test %>% filter(strata == "ncompetitor=1")

#model_PIV_frailty_GT_adj_post <- coxph(Surv(genericPIV$gaptime_start, genericPIV$gaptime, entry2) ~ strata(ncompetitor) + route + AG + Tier + ATC1 + guidance_before + indexyear + frailty(index), data = genericPIV)
#summary(model_PIV_frailty_GT_adj_post)
#h_test <- basehaz(model_PIV_frailty_GT_adj_post, centered = T)



h_test1_long <- melt(h_test1, id = "time", measure = c("hazard", "h2"))

h_test1_long$tend <- c(h_test1_long$time[2:nrow(h_test1)], NA)

p1 <- (ggplot(h_test1_long, aes(x=time, y=value, xend=tend, yend=value, color = variable)) +
         geom_vline(aes(xintercept=time), linetype=2, color="grey") +
         geom_point() +  # Solid points to left
         geom_point(aes(x=tend, y=value), shape=1) +  # Open points to right
         geom_segment())  # Horizontal line segments
p1

h_test2 <- h_test %>% filter(strata == "ncompetitor=2")

h_test2_long <- melt(h_test2, id = "time", measure = c("hazard", "h2"))

h_test2_long$tend <- c(h_test2_long$time[2:nrow(h_test2)], NA)

p2 <- (ggplot(h_test2_long, aes(x=time, y=value, xend=tend, yend=value, color = variable)) +
         geom_vline(aes(xintercept=time), linetype=2, color="grey") +
         geom_point() +  # Solid points to left
         geom_point(aes(x=tend, y=value), shape=1) +  # Open points to right
         geom_segment())  # Horizontal line segments
p2

h_test3 <- h_test %>% filter(strata == "ncompetitor=3")

h_test3_long <- melt(h_test3, id = "time", measure = c("hazard", "h2"))

h_test3_long$tend <- c(h_test3_long$time[2:nrow(h_test3)], NA)

p3 <- (ggplot(h_test3_long, aes(x=time, y=value, xend=tend, yend=value, color = variable)) +
         geom_vline(aes(xintercept=time), linetype=2, color="grey") +
         geom_point() +  # Solid points to left
         geom_point(aes(x=tend, y=value), shape=1) +  # Open points to right
         geom_segment())  # Horizontal line segments
p3



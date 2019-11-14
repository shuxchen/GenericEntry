####no PIV###########################################################
##For each index, keep if PIV==0
generic$PIV[is.na(generic$PIV)]<-0
genericnoPIV<-generic %>% group_by(index) %>% filter(all(PIV==0))
#Apply patent expiration date to everyone within group
#Calculate time difference
#require(tidyverse)
#genericnoPIV <- genericPIV %>% group_by(index) %>% fill(exclusivity) %>% fill(exclusivity, .direction = "up")
#genericPIV$lag<-as.Date(genericPIV$date)-as.Date(genericPIV$exclusivity)
#genericPIV$lag[genericPIV$PIV==1]<-0
#Delete those with negative lag time???
#genericPIV<-genericPIV[genericPIV[, "lag"]>=0, ]

#Rank by time of approval for the order of entry within group: entry
#Drugs entering at the same time would receive the same ranking/entry
#genericnoPIV <- genericnoPIV %>% arrange(index, date) %>% group_by(index) %>%
#  ungroup()  %>% as.data.frame()

#genericnoPIV <- genericnoPIV %>%
#  group_by(index) %>%
#  mutate(entry = rank(numdate,ties.method="min"))
#If different rank for drugs entering at the same time
#genericnoPIV$order <- sequence(tabulate(genericnoPIV$index)) 
#genericnoPIV <- genericnoPIV %>%
#  group_by(index) %>%
#  mutate(order = rank(numdate,ties.method="first"))

#add restrict for index date >= 20071001
genericnoPIV <- genericnoPIV %>% 
  group_by(index) %>% 
  fill(min) %>% 
  fill(min, .direction = "up")

genericnoPIV <- genericnoPIV %>%
  filter(min >= "2007-10-01")

genericnoPIV_origin <- genericnoPIV

##add administrative censoring for pre-GDUFA index dates
#for index date pre-GDUFA, exclude generics with entry date post-GDUFA
genericnoPIV_preGDUFA <- genericnoPIV %>%
  filter(min < '2012-10-01' & date < '2012-10-01')

#Here, missing cases when min < '2012-10-01' and first entrant's date >='2012-10-01'; need to add branded drug as censored

genericnoPIV <- genericnoPIV %>%
  filter(min >= '2012-10-01') %>%
  bind_rows(genericnoPIV_preGDUFA)

#Calculate lag time using earliest patent
genericnoPIV$lag <- as.Date(genericnoPIV$date)-as.Date(genericnoPIV$min)

firstnoPIV <- genericnoPIV %>% filter(order == 1)
sum(is.na(firstnoPIV$min))/nrow(firstnoPIV)

#Delete those with negative or NA lag time
genericnoPIV <- genericnoPIV %>% filter(lag>=0)



#Update order
genericnoPIV <- genericnoPIV %>% group_by(index) %>% arrange(numdate)
genericnoPIV <- genericnoPIV %>%
  group_by(index) %>% 
  mutate(order = rank(numdate,ties.method="first"))

#Constructing variable for at least one entry:
genericnoPIV$entry1 <- 1

#construct variables for start time and stop time
genericnoPIV <- genericnoPIV %>% arrange(index, order) 
genericnoPIV <- genericnoPIV %>% group_by(index) %>% mutate(t0 = lag(lag), t1 = lag, gaptime = lag - lag(lag))

#first entrant - assign t0 to be 0, gaptime to be lag
genericnoPIV$t0[genericnoPIV$order == 1] <- 0
genericnoPIV$gaptime[genericnoPIV$order == 1] <- genericnoPIV$lag[genericnoPIV$order == 1]

#Add GDUFA effects
genericnoPIV$GDUFA <- 0
genericnoPIV$GDUFA[genericnoPIV$numdate >= 20121001] <- 1
genericnoPIV$indexyear <- round(as.integer(format(as.Date(genericnoPIV$min), "%Y%m%d"))/10000)


genericnoPIV_nocensor <- genericnoPIV

#Recode order of entry
genericnoPIV$ncompetitor <- genericnoPIV$order - 1

#count of generics
genericnoPIV_index <- genericnoPIV %>% group_by(index) %>% summarise(count_generic=n()) %>% as.data.frame()

genericnoPIV <- left_join(genericnoPIV, genericnoPIV_index)


#Add branded drugs without any generic competition
branded_nogeneric <- df %>% filter(Appl_Type=="N" & count == 1)
#Only include those with earliest patent expiration after 2007-10-01
#So max follow-up time would be 10 years (length of study interest)
#Only include those with earliest patent expiration before 2017-10-01 
#So there's risk of entry after patent expiration, not PIV
branded_nogeneric <- branded_nogeneric %>% filter(min >= "2007-10-01" & min <= "2017-09-30")

branded_nogeneric$entry1 <- 0
branded_nogeneric$t0 <- 0

branded_nogeneric_preGDUFA <- branded_nogeneric %>%
  filter(min < '2012-10-01') 

branded_nogeneric_postGDUFA <- branded_nogeneric %>%
  filter(min >= '2012-10-01') 

#Different admin censoring dates for pre and post GDUFA 
branded_nogeneric_preGDUFA$t1 <- as.numeric(as.Date("2012-09-30") - as.Date(branded_nogeneric_preGDUFA$min))
branded_nogeneric_postGDUFA$t1 <- as.numeric(as.Date("2017-09-30") - as.Date(branded_nogeneric_postGDUFA$min))

branded_nogeneric <- branded_nogeneric_preGDUFA %>%
  bind_rows(branded_nogeneric_postGDUFA)

#also add branded drugs with min <= 2012-10-01, but have first generic entry after this
branded_preGDUFA <- genericnoPIV_origin %>%
  filter(min < '2012-10-01' & date >= '2012-10-01' & order == 1)
branded_preGDUFA$entry1 <- 0
branded_preGDUFA$t0 <- 0
branded_preGDUFA$gaptime <- branded_preGDUFA$t1 <- as.numeric(as.Date("2012-09-30") - as.Date(branded_preGDUFA$min))
branded_nogeneric <- branded_nogeneric %>%
  bind_rows(branded_preGDUFA)

#branded_nogeneric$t1[branded_nogeneric$min >= "2012-10-01"] <- as.numeric(as.Date("2017-09-30") - as.Date(branded_nogeneric$date[branded_nogeneric$min >= "2012-10-01"]))
#branded_nogeneric$t1[branded_nogeneric$min < "2012-10-01"] <- as.numeric(as.Date("2012-09-30") - as.Date(branded_nogeneric$date[branded_nogeneric$min < "2012-10-01"]))
branded_nogeneric$gaptime <- branded_nogeneric$t1
branded_nogeneric$approveyear <- floor(branded_nogeneric$numdate/10000)
branded_nogeneric$GDUFA <- 0
branded_nogeneric$GDUFA[branded_nogeneric$min >= "2012-10-01" ] <- 1
branded_nogeneric$ncompetitor <- 0
branded_nogeneric$order <- 0

genericnoPIV$t0 <- as.numeric(genericnoPIV$t0)
genericnoPIV$t1 <- as.numeric(genericnoPIV$t1)
genericnoPIV$gaptime <- as.numeric(genericnoPIV$gaptime)

table(genericnoPIV$ATC1, genericnoPIV$GDUFA)
ATC_noPIV <- data.frame(table(genericnoPIV$ATC1, genericnoPIV$GDUFA))
colnames(ATC_noPIV)[1] <- "ATC1"
colnames(ATC_noPIV)[2] <- "GDUFA"
colnames(ATC_noPIV)[3] <- "Number"


g4 <- ggplot(ATC_noPIV, aes(x = GDUFA, y = Number, col = ATC1)) +
  geom_point()

#Add censored information
#use different admin censoring date for pre and post GDUFA 
genericnoPIV_censor <- genericnoPIV %>% filter(order == count_generic)
genericnoPIV_censor <- genericnoPIV_censor %>% filter(!entry1 == 0)
genericnoPIV_censor$entry1 <- 0
genericnoPIV_censor$ncompetitor <- genericnoPIV_censor$ncompetitor + 1
genericnoPIV_censor$t0 <- genericnoPIV_censor$t1

genericnoPIV_censor_preGDUFA <- genericnoPIV_censor %>%
  filter(min < '2012-10-01') 

genericnoPIV_censor_postGDUFA <- genericnoPIV_censor %>%
  filter(min >= '2012-10-01') 

genericnoPIV_censor_preGDUFA$t1<- as.numeric(as.Date("2012-09-30") - as.Date(genericnoPIV_censor_preGDUFA$min))
genericnoPIV_censor_postGDUFA$t1<- as.numeric(as.Date("2017-09-30") - as.Date(genericnoPIV_censor_postGDUFA$min))

genericnoPIV_censor <- genericnoPIV_censor_preGDUFA %>%
  bind_rows(genericnoPIV_censor_postGDUFA)

#genericnoPIV_censor$t1[genericnoPIV_censor$min >= "2012-10-01"] <- as.numeric(as.Date("2017-09-30") - as.Date(genericnoPIV_censor$min[genericnoPIV_censor$min >= "2012-10-01"]))
#genericnoPIV_censor$t1[genericnoPIV_censor$min < "2012-10-01"] <- as.numeric(as.Date("2012-09-30") - as.Date(genericnoPIV_censor$min[genericnoPIV_censor$min < "2012-10-01"]))

genericnoPIV_censor$gaptime <- as.numeric(as.Date(genericnoPIV_censor$t1) - as.Date(genericnoPIV_censor$t0))

#merge back to main dataset
genericnoPIV <- rbind(genericnoPIV, genericnoPIV_censor)

#Merge censored generic in
genericnoPIV <- rbind(genericnoPIV, branded_nogeneric)


#Make year 2012 the reference year
genericnoPIV$approveyear <- as.factor(genericnoPIV$approveyear)
genericnoPIV$approveyear <- relevel(genericnoPIV$approveyear, ref = "2012")

#Generate index year (exclusivity year)
genericnoPIV$indexyear <- round(as.integer(format(as.Date(genericnoPIV$min), "%Y%m%d"))/10000)

#Make year 2012 the reference year
genericnoPIV$indexyear_cat <- as.factor(genericnoPIV$indexyear)
genericnoPIV$indexyear_cat <- relevel(genericnoPIV$indexyear_cat, ref = "2012")
genericnoPIV$indexyear <- genericnoPIV$indexyear - 2012


#genericnoPIV$indexyear <- as.numeric(genericnoPIV$indexyear) - 2012

#Keep k <= 6
genericnoPIV <- genericnoPIV %>% filter(order <= 6)
genericnoPIV <- genericnoPIV %>% filter(ncompetitor <= 5)


#Start time = stop time will be excluded, try:
genericnoPIV$t1[genericnoPIV$t0 == genericnoPIV$t1] <- as.numeric(genericnoPIV$t1[genericnoPIV$t0 == genericnoPIV$t1]) + 0.001

model_noPIV_test_unadj <- coxph(Surv(t0, t1, entry1) ~ GDUFA*as.factor(ncompetitor) + cluster(index), method = "breslow", data = genericnoPIV)
summary(model_noPIV_test_unadj) 

model_noPIV_test_adj <- coxph(Surv(t0, t1, entry1) ~ GDUFA*as.factor(ncompetitor) + route + AG + ETASU + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericnoPIV)
summary(model_noPIV_test_adj) 


#Stratified PWP-TT
model_noPIV_PWPTT_unadj <- coxph(Surv(t0, t1, entry1) ~ GDUFA*strata(ncompetitor) + cluster(index), method = "breslow", data = genericnoPIV)
summary(model_noPIV_PWPTT_unadj)

model_noPIV_PWPTT_adj <- coxph(Surv(t0, t1, entry1) ~ GDUFA*strata(ncompetitor) + route + AG + ETASU + guidance_before  + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericnoPIV)
#model_noPIV_PWPTT_adj <- coxph(Surv(t0, t1, entry1) ~ (GDUFA + route + AG + ATC1 + ETASU + guidance_before  + indexyear)*strata(ncompetitor) + cluster(index), method = "breslow", data = genericnoPIV)
summary(model_noPIV_PWPTT_adj)


#Stratified PWP-GT
genericnoPIV$gaptime_start <- 0
genericnoPIV$gaptime[genericnoPIV$gaptime == 0] <- as.numeric(genericnoPIV$gaptime[genericnoPIV$gaptime == 0]) + 0.001

model_noPIV_PWPGT_unadj <- coxph(Surv(genericnoPIV$gaptime_start, gaptime, entry1) ~ GDUFA*strata(ncompetitor) + cluster(index), method = "breslow", data = genericnoPIV)
summary(model_noPIV_PWPGT_unadj)

model_noPIV_PWPGT_adj <- coxph(Surv(genericnoPIV$gaptime_start, gaptime, entry1) ~ GDUFA*strata(ncompetitor) + route + AG + ETASU + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericnoPIV)
#model_noPIV_PWPGT_adj <- coxph(Surv(genericnoPIV$gaptime_start, gaptime, entry1) ~ (GDUFA + route + AG + ATC1 + ETASU + guidance_before + indexyear)*strata(ncompetitor) + cluster(index), method = "breslow", data = genericnoPIV)

summary(model_noPIV_PWPGT_adj)


#Model with # of competitors not in strata
model_noPIV_test <- coxph(Surv(t0, t1, entry1) ~ GDUFA*as.factor(ncompetitor) + route + AG + ATC1 + ETASU + guidance_before  + cluster(index), method = "breslow", data = genericnoPIV)
summary(model_noPIV_test)

#adding frailty to PWP-TT
model_noPIV_PWPTT_frailty_unadj <- coxph(Surv(t0, t1, entry1) ~ GDUFA*strata(ncompetitor) + frailty(index), method = "breslow", robust = T, data = genericnoPIV)
summary(model_noPIV_PWPTT_frailty_unadj)

model_noPIV_PWPTT_frailty_adj <- coxph(Surv(t0, t1, entry1) ~ GDUFA*strata(ncompetitor) + route + AG + ETASU + guidance_before + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV  + frailty(index), method = "breslow", robust = T, data = genericnoPIV)
summary(model_noPIV_PWPTT_frailty_adj)

#adding frailty to PWP-GT
model_noPIV_frailty_unadj <- coxph(Surv(genericnoPIV$gaptime_start, genericnoPIV$gaptime, entry1) ~ GDUFA*strata(ncompetitor) + frailty(index), data = genericnoPIV)

summary(model_noPIV_frailty_unadj)

model_noPIV_frailty_GT_adj <- coxph(Surv(genericnoPIV$gaptime_start, genericnoPIV$gaptime, entry1) ~ GDUFA*strata(ncompetitor) + route + AG + ETASU + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + frailty(index), data = genericnoPIV)

summary(model_noPIV_frailty_GT_adj)

model_noPIV_frailty_TT_unadj <- coxme(Surv(t0, t1, entry1) ~ GDUFA*strata(ncompetitor) + (1|index), data = genericnoPIV)

summary(model_noPIV_frailty_TT_unadj)

model_noPIV_frailty_TT_adj <- coxme(Surv(t0, t1, entry1) ~ GDUFA*strata(ncompetitor) + route + AG + ETASU + guidance_before + indexyear+ ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV  +  (1|index), data = genericnoPIV)

summary(model_noPIV_frailty_TT_adj)

coef <- summary(model_noPIV_frailty_TT_adj)$coef
se <- extractSE(model_noPIV_frailty_TT_adj)
exp(coef-1.96*se)
exp(coef+1.96*se)

###Limit to 1500 days
genericnoPIV_1500 <- genericnoPIV %>% filter(lag <= 1500)
##PWP-GT
model_noPIV_1500_PWPGT_adj <- coxph(Surv(genericnoPIV_1500$gaptime_start, genericnoPIV_1500$gaptime, entry1) ~ GDUFA*strata(ncompetitor) + route + AG + ETASU + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericnoPIV_1500)
summary(model_noPIV_1500_PWPGT_adj)
##PWP-TT
model_noPIV_1500_PWPTT_adj <- coxph(Surv(t0, t1, entry1) ~ GDUFA*strata(ncompetitor) + route + AG + ETASU + guidance_before  + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericnoPIV_1500)
summary(model_noPIV_1500_PWPTT_adj)


##Exclude index date 6-month before GDUFA
genericnoPIV_6mon <- genericnoPIV %>% filter(min < "2012-04-01" | min >= "2012-10-01" )
##PWP-GT
model_noPIV_6mon_PWPGT_adj <- coxph(Surv(gaptime_start, gaptime, entry1) ~ GDUFA*strata(ncompetitor) + route + AG + ETASU + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericnoPIV_6mon)
summary(model_noPIV_6mon_PWPGT_adj)
##PWP-TT
model_noPIV_6mon_PWPTT_adj <- coxph(Surv(t0, t1, entry1) ~ GDUFA*strata(ncompetitor) + route + AG + ETASU + guidance_before  + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericnoPIV_6mon)
summary(model_noPIV_6mon_PWPTT_adj)


##Adjust for seasonality
genericnoPIV$month <- genericnoPIV$approvemonth%%100
##PWP-GT
model_noPIV_season_PWPGT_adj <- coxph(Surv(gaptime_start, gaptime, entry1) ~ GDUFA*strata(ncompetitor) + route + AG + ETASU + guidance_before + indexyear + as.factor(month) + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericnoPIV)
summary(model_noPIV_season_PWPGT_adj)
##PWP-TT
model_noPIV_season_PWPTT_adj <- coxph(Surv(t0, t1, entry1) ~ GDUFA*strata(ncompetitor) + route + AG + ETASU + guidance_before  + indexyear + as.factor(month)  + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericnoPIV)
summary(model_noPIV_season_PWPTT_adj)

##Adjust for ACA
#+ ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV 
genericnoPIV <- genericnoPIV %>% mutate(ACA = ifelse(numdate >= 20140101, 1, 0))

model_noPIV_PWPTT_ACA_adj <- coxph(Surv(t0, t1, entry1) ~ GDUFA*strata(ncompetitor) + route + AG + ETASU + guidance_before  + indexyear + ACA + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericnoPIV)
summary(model_noPIV_PWPTT_ACA_adj)

model_noPIV_PWPGT_ACA_adj <- coxph(Surv(genericnoPIV$gaptime_start, gaptime, entry1) ~ GDUFA*strata(ncompetitor) + route + AG + ETASU + guidance_before + indexyear + ACA + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV + cluster(index), method = "breslow", data = genericnoPIV)
summary(model_noPIV_PWPGT_ACA_adj)


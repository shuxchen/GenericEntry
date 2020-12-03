load("all.RData")
load("generic.RData")

###PIV#################################################################
##For each index, keep if PIV=1 
##This is the dataset with generic entry related to first filer entering with PIV
generic$PIV[is.na(generic$PIV)]<-0
genericnoPIV<-generic %>% group_by(index) %>% filter(all(PIV==0))

genericnoPIV <- genericnoPIV %>% 
  group_by(index) %>% 
  fill(min) %>% 
  fill(min, .direction = "up")

# re-calculate index date, by using min(earliest generic entry, earliest patent expiration date)
genericnoPIV_min_approvedate <- genericnoPIV %>% 
  group_by(index) %>% 
  summarise(min_generic = min(date))

#merge back 
#compare min and min_generic, use the smaller one; also fill NA patent expiry with earliest generic entry date
genericnoPIV <- genericnoPIV %>% 
  left_join(genericnoPIV_min_approvedate) %>%
  mutate(min_new = dplyr::if_else(is.na(min) | as.Date(min) > as.Date(min_generic), as.Date(min_generic), as.Date(min))) %>%
  dplyr::select(-min_generic, -min) %>%
  rename(min = min_new)


#add restrict for index date >= 20071001
genericnoPIV <- genericnoPIV %>%
  filter(min >= "2007-10-01")

genericnoPIV_origin <- genericnoPIV

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

save(genericnoPIV_origin, file = "genericnoPIV.RData")


#1. constructing variable for at least one entry:
genericnoPIV$entry1 <- 1

#2. Construct start and end time (gap time)
genericnoPIV <- genericnoPIV %>% arrange(index, order) 
genericnoPIV <- genericnoPIV %>% group_by(index) %>% mutate(t0 = lag(lag), t1 = lag, gaptime = lag - lag(lag))

genericnoPIV_index <- genericnoPIV %>% group_by(index) %>% summarise(count_generic=n()) %>% as.data.frame()

genericnoPIV <- left_join(genericnoPIV, genericnoPIV_index)

#first entrant - assign t0 to be 0, gaptime to be lag
genericnoPIV$t0[genericnoPIV$order == 1] <- 0
genericnoPIV$gaptime[genericnoPIV$order == 1] <- genericnoPIV$lag[genericnoPIV$order == 1]

genericnoPIV$GDUFA <- 0
genericnoPIV$GDUFA[genericnoPIV$numdate >= 20121001] <- 1
genericnoPIV$indexyear <- round(as.integer(format(as.Date(genericnoPIV$min), "%Y%m%d"))/10000)

table(genericnoPIV$ATC1, genericnoPIV$GDUFA)
ATC_PIV <- data.frame(table(genericnoPIV$ATC1, genericnoPIV$GDUFA))
colnames(ATC_PIV)[1] <- "ATC1"
colnames(ATC_PIV)[2] <- "GDUFA"
colnames(ATC_PIV)[3] <- "Number"

#Generate index year (min year)
genericnoPIV$indexyear <- round(as.integer(format(as.Date(genericnoPIV$min), "%Y%m%d"))/10000)
#Make year 2012 the reference year
genericnoPIV$indexyear_cat <- as.factor(genericnoPIV$indexyear)
genericnoPIV$indexyear_cat <- relevel(genericnoPIV$indexyear_cat, ref = "2012")
genericnoPIV$indexyear <- genericnoPIV$indexyear - 2012

genericnoPIV$ncompetitor <- genericnoPIV$order - 1

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

branded_nogeneric$gaptime <- branded_nogeneric$t1
branded_nogeneric$approveyear <- floor(branded_nogeneric$numdate/10000)
branded_nogeneric$GDUFA <- 0
branded_nogeneric$GDUFA[branded_nogeneric$min >= "2012-10-01" ] <- 1
branded_nogeneric$ncompetitor <- 0
branded_nogeneric$order <- 0
branded_nogeneric$indexyear <- round(as.integer(format(as.Date(branded_nogeneric$min), "%Y%m%d"))/10000)


genericnoPIV$t0 <- as.numeric(genericnoPIV$t0)
genericnoPIV$t1 <- as.numeric(genericnoPIV$t1)
genericnoPIV$gaptime <- as.numeric(genericnoPIV$gaptime)

#genericnoPIV <- rbind(genericnoPIV, branded_nogeneric)


genericnoPIV$t0[is.na(genericnoPIV$t0)] <- 0

genericnoPIV$GDUFA <- 0
genericnoPIV$GDUFA[genericnoPIV$numdate >= 20121001] <- 1

#Recode order of entry
genericnoPIV$ncompetitor <- genericnoPIV$order - 1
genericnoPIV$ncompetitor[genericnoPIV$order == 1] <- 0
genericnoPIV$ncompetitor[genericnoPIV$order == 0] <- 0


#Limit to 6 entrants
genericnoPIV <- genericnoPIV %>%
  filter(order <= 6) 

# create obs for 5 entrants only, regarding the 6th
genericnoPIV_5 <- genericnoPIV %>%
  filter(count_generic == 5 & order == 5)

genericnoPIV_5_6 <- genericnoPIV_5 %>%
  mutate(ncompetitor = 5,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         entry1 = 0)

# create obs for 4 entrants only, regarding the 5th and 6th
genericnoPIV_4 <- genericnoPIV %>%
  filter(count_generic == 4 & order == 4)   

genericnoPIV_4_6 <- genericnoPIV_4 %>%
  mutate(ncompetitor = 5,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         entry1 = 0)

genericnoPIV_4_5 <- genericnoPIV_4 %>%
  mutate(ncompetitor = 4,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         entry1 = 0)

# create obs for 3 entrants only, regarding the 4th, 5th and 6th
genericnoPIV_3 <- genericnoPIV %>%
  filter(count_generic == 3 & order == 3)   

genericnoPIV_3_6 <- genericnoPIV_3 %>%
  mutate(ncompetitor = 5,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         entry1 = 0)

genericnoPIV_3_5 <- genericnoPIV_3 %>%
  mutate(ncompetitor = 4,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         entry1 = 0)

genericnoPIV_3_4 <- genericnoPIV_3 %>%
  mutate(ncompetitor = 3,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         entry1 = 0)

# create obs for 2 entrants only, regarding the 3rd, 4th, 5th and 6th
genericnoPIV_2 <- genericnoPIV %>%
  filter(count_generic == 2 & order == 2)   

genericnoPIV_2_6 <- genericnoPIV_2 %>%
  mutate(ncompetitor = 5,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         entry1 = 0)

genericnoPIV_2_5 <- genericnoPIV_2 %>%
  mutate(ncompetitor = 4,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         entry1 = 0)

genericnoPIV_2_4 <- genericnoPIV_2 %>%
  mutate(ncompetitor = 3,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         entry1 = 0)

genericnoPIV_2_3 <- genericnoPIV_2 %>%
  mutate(ncompetitor = 2,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         entry1 = 0)

# create obs for 1 entrant only, regarding the 2nd, 3rd, 4th, 5th and 6th
genericnoPIV_1 <- genericnoPIV %>%
  filter(count_generic == 1 & order == 1)   

genericnoPIV_1_6 <- genericnoPIV_1 %>%
  mutate(ncompetitor = 5,
         t0 = 0,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         gaptime = t1,
         entry1 = 0)

genericnoPIV_1_5 <- genericnoPIV_1 %>%
  mutate(ncompetitor = 4,
         t0 = 0,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         gaptime = t1,
         entry1 = 0)

genericnoPIV_1_4 <- genericnoPIV_1 %>%
  mutate(ncompetitor = 3,
         t0 = 0,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         gaptime = t1,
         entry1 = 0)

genericnoPIV_1_3 <- genericnoPIV_1 %>%
  mutate(ncompetitor = 2,
         t0 = 0,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         gaptime = t1,
         entry1 = 0)

genericnoPIV_1_2 <- genericnoPIV_1 %>%
  mutate(ncompetitor = 1,
         t0 = 0,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         gaptime = t1,
         entry1 = 0)

# create obs for branded only, regarding the 1st, 2nd, 3rd, 4th, 5th and 6th
genericnoPIV_0 <- branded_nogeneric

genericnoPIV_0_6 <- genericnoPIV_0 %>%
  mutate(ncompetitor = 5,
         t0 = 0,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         gaptime = t1,
         entry1 = 0)

genericnoPIV_0_5 <- genericnoPIV_0 %>%
  mutate(ncompetitor = 4,
         t0 = 0,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         gaptime = t1,
         entry1 = 0)

genericnoPIV_0_4 <- genericnoPIV_0 %>%
  mutate(ncompetitor = 3,
         t0 = 0,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         gaptime = t1,
         entry1 = 0)

genericnoPIV_0_3 <- genericnoPIV_0 %>%
  mutate(ncompetitor = 2,
         t0 = 0,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         gaptime = t1,
         entry1 = 0)

genericnoPIV_0_2 <- genericnoPIV_0 %>%
  mutate(ncompetitor = 1,
         t0 = 0,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         gaptime = t1,
         entry1 = 0)

genericnoPIV_0_1 <- genericnoPIV_0 %>%
  mutate(ncompetitor = 0,
         t0 = 0,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(min)), as.numeric(as.Date("2012-09-30") - as.Date(min))),
         gaptime = t1,
         entry1 = 0)

#also add branded drugs with min <= 2012-10-01, but have first generic entry after this
branded_preGDUFA <- genericnoPIV_origin %>%
  filter(min < '2012-10-01' & date >= '2012-10-01' & order == 1)
branded_preGDUFA$entry1 <- 0
branded_preGDUFA$t0 <- 0
branded_preGDUFA$gaptime <- branded_preGDUFA$t1 <- as.numeric(as.Date("2012-09-30") - as.Date(branded_preGDUFA$min))

branded_preGDUFA$GDUFA <- 1
branded_preGDUFA$indexyear <- round(as.integer(format(as.Date(branded_preGDUFA$min), "%Y%m%d"))/10000)


branded_preGDUFA_6 <- branded_preGDUFA %>%
  mutate(ncompetitor = 5)
branded_preGDUFA_5 <- branded_preGDUFA %>%
  mutate(ncompetitor = 4)
branded_preGDUFA_4 <- branded_preGDUFA %>%
  mutate(ncompetitor = 3)
branded_preGDUFA_3 <- branded_preGDUFA %>%
  mutate(ncompetitor = 2)
branded_preGDUFA_2 <- branded_preGDUFA %>%
  mutate(ncompetitor = 1)
branded_preGDUFA_1 <- branded_preGDUFA %>%
  mutate(ncompetitor = 0)

genericnoPIV_WLW <- genericnoPIV %>%
  rbind(genericnoPIV_0_1) %>%
  rbind(genericnoPIV_0_2) %>%
  rbind(genericnoPIV_0_3) %>%
  rbind(genericnoPIV_0_4) %>%
  rbind(genericnoPIV_0_5) %>%
  rbind(genericnoPIV_0_6) %>%
  rbind(genericnoPIV_1_2) %>%
  rbind(genericnoPIV_1_3) %>%
  rbind(genericnoPIV_1_4) %>%
  rbind(genericnoPIV_1_5) %>%
  rbind(genericnoPIV_1_6) %>%
  rbind(genericnoPIV_2_3) %>%
  rbind(genericnoPIV_2_4) %>%
  rbind(genericnoPIV_2_5) %>% 
  rbind(genericnoPIV_2_6) %>% 
  rbind(genericnoPIV_3_4) %>%
  rbind(genericnoPIV_3_5) %>% 
  rbind(genericnoPIV_3_6) %>%   
  rbind(genericnoPIV_4_5) %>% 
  rbind(genericnoPIV_4_6) %>% 
  rbind(genericnoPIV_5_6) %>%
  rbind(branded_preGDUFA_1) %>%
  rbind(branded_preGDUFA_2) %>%
  rbind(branded_preGDUFA_3) %>%
  rbind(branded_preGDUFA_4) %>%
  rbind(branded_preGDUFA_5) %>%
  rbind(branded_preGDUFA_6) 

genericnoPIV_WLW$t1[genericnoPIV_WLW$t0 == genericnoPIV_WLW$t1] <- as.numeric(genericnoPIV_WLW$t1[genericnoPIV_WLW$t0 == genericnoPIV_WLW$t1]) + 0.001


model_noPIV_WLW <- coxph(Surv(t0, t1, entry1) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV  + cluster(index), method = "breslow", data = genericnoPIV_WLW)
model_noPIV_WLW <- coxph(Surv(t1, entry1) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV  + cluster(index), method = "breslow", robust = TRUE, data = genericnoPIV_WLW)
summary(model_noPIV_WLW)
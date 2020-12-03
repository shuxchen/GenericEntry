load("all.RData")
load("generic.RData")

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

#if PIV date before exclusivity, use exclusivity end date instead (replace)
genericPIV$date[genericPIV$PIV==1]<-genericPIV$exclusivity

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

save(genericPIV_origin, file = "genericPIV.RData")


#1. exclude first entrants 
genericPIV$entry2 <- 1

#2. Construct start and end time (gap time)
genericPIV <- genericPIV %>% arrange(index, order) 
genericPIV <- genericPIV %>% group_by(index) %>% mutate(t0 = lag(lag), t1 = lag, gaptime = lag - lag(lag))

genericPIV_index <- genericPIV %>% group_by(index) %>% summarise(count_generic=n()) %>% as.data.frame()

genericPIV <- left_join(genericPIV, genericPIV_index)

genericPIV$GDUFA <- 0
genericPIV$GDUFA[genericPIV$numdate >= 20121001] <- 1
genericPIV$indexyear <- round(as.integer(format(as.Date(genericPIV$exclusivity), "%Y%m%d"))/10000)

table(genericPIV$ATC1, genericPIV$GDUFA)
ATC_PIV <- data.frame(table(genericPIV$ATC1, genericPIV$GDUFA))
colnames(ATC_PIV)[1] <- "ATC1"
colnames(ATC_PIV)[2] <- "GDUFA"
colnames(ATC_PIV)[3] <- "Number"

#Generate index year (exclusivity year)
genericPIV$indexyear <- round(as.integer(format(as.Date(genericPIV$exclusivity), "%Y%m%d"))/10000)
#Make year 2012 the reference year
genericPIV$indexyear_cat <- as.factor(genericPIV$indexyear)
genericPIV$indexyear_cat <- relevel(genericPIV$indexyear_cat, ref = "2012")
genericPIV$indexyear <- genericPIV$indexyear - 2012


genericPIV_first <- genericPIV %>% 
  filter(order == 1)
genericPIV_first_only <- genericPIV_first %>% 
  filter(count_generic == 1) 

#for these ones, use start time = 0, calculate stop time as "2017-09-31 - approval date"
genericPIV_first_only$t0 <- 0
genericPIV <- genericPIV %>%
  filter(order != 1)

genericPIV_first_only_preGDUFA <- genericPIV_first_only %>%
  filter(exclusivity < '2012-10-01') 

genericPIV_first_only_postGDUFA <- genericPIV_first_only %>%
  filter(exclusivity >= '2012-10-01' & date >= '2012-10-01' ) 

genericPIV_first_only_preGDUFA$gaptime <- genericPIV_first_only_preGDUFA$t1 <- as.numeric(as.Date("2012-09-30") - as.Date(genericPIV_first_only_preGDUFA$date))
genericPIV_first_only_postGDUFA$gaptime <- genericPIV_first_only_postGDUFA$t1 <- as.numeric(as.Date("2017-09-30") - as.Date(genericPIV_first_only_postGDUFA$date))

genericPIV_first_only <- genericPIV_first_only_preGDUFA %>%
  bind_rows(genericPIV_first_only_postGDUFA)

genericPIV_first_only$GDUFA[genericPIV_first_only$date >= "2012-10-01" ] <- 1

genericPIV <- genericPIV %>%
  bind_rows(genericPIV_first_only)

genericPIV$t0 <- as.numeric(genericPIV$t0)
genericPIV$t1 <- as.numeric(genericPIV$t1)
genericPIV$gaptime <- as.numeric(genericPIV$gaptime)


#genericPIV <- rbind(genericPIV, genericPIV_first_only)

genericPIV$t0[is.na(genericPIV$t0)] <- 0

genericPIV$GDUFA <- 0
genericPIV$GDUFA[genericPIV$numdate >= 20121001] <- 1

#Recode order of entry
genericPIV$ncompetitor <- genericPIV$order - 1
#genericPIV$ncompetitor[genericPIV$order == 1] <- 1


#Limit to 6 entrants
genericPIV <- genericPIV %>%
  filter(order <= 6) 

# create obs for 5 entrants only, regarding the 6th
genericPIV_5 <- genericPIV %>%
  filter(count_generic == 5 & order == 5)

genericPIV_5_6 <- genericPIV_5 %>%
  mutate(ncompetitor = 5,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(exclusivity)), as.numeric(as.Date("2012-09-30") - as.Date(exclusivity))),
         entry2 = 0)

# create obs for 4 entrants only, regarding the 5th and 6th
genericPIV_4 <- genericPIV %>%
  filter(count_generic == 4 & order == 4)   

genericPIV_4_6 <- genericPIV_4 %>%
  mutate(ncompetitor = 5,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(exclusivity)), as.numeric(as.Date("2012-09-30") - as.Date(exclusivity))),
         entry2 = 0)

genericPIV_4_5 <- genericPIV_4 %>%
  mutate(ncompetitor = 4,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(exclusivity)), as.numeric(as.Date("2012-09-30") - as.Date(exclusivity))),
         entry2 = 0)

# create obs for 3 entrants only, regarding the 4th, 5th and 6th
genericPIV_3 <- genericPIV %>%
  filter(count_generic == 3 & order == 3)   

genericPIV_3_6 <- genericPIV_3 %>%
  mutate(ncompetitor = 5,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(exclusivity)), as.numeric(as.Date("2012-09-30") - as.Date(exclusivity))),
         entry2 = 0)

genericPIV_3_5 <- genericPIV_3 %>%
  mutate(ncompetitor = 4,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(exclusivity)), as.numeric(as.Date("2012-09-30") - as.Date(exclusivity))),
         entry2 = 0)

genericPIV_3_4 <- genericPIV_3 %>%
  mutate(ncompetitor = 3,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(exclusivity)), as.numeric(as.Date("2012-09-30") - as.Date(exclusivity))),
         entry2 = 0)

# create obs for 2 entrants only, regarding the 3rd, 4th, 5th and 6th
genericPIV_2 <- genericPIV %>%
  filter(count_generic == 2 & order == 2)   

genericPIV_2_6 <- genericPIV_2 %>%
  mutate(ncompetitor = 5,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(exclusivity)), as.numeric(as.Date("2012-09-30") - as.Date(exclusivity))),
         entry2 = 0)

genericPIV_2_5 <- genericPIV_2 %>%
  mutate(ncompetitor = 4,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(exclusivity)), as.numeric(as.Date("2012-09-30") - as.Date(exclusivity))),
         entry2 = 0)

genericPIV_2_4 <- genericPIV_2 %>%
  mutate(ncompetitor = 3,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(exclusivity)), as.numeric(as.Date("2012-09-30") - as.Date(exclusivity))),
         entry2 = 0)

genericPIV_2_3 <- genericPIV_2 %>%
  mutate(ncompetitor = 2,
         t0 = t1,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(exclusivity)), as.numeric(as.Date("2012-09-30") - as.Date(exclusivity))),
         entry2 = 0)

# create obs for 1 entrant only, regarding the 2nd, 3rd, 4th, 5th and 6th
genericPIV_1 <- genericPIV_first_only  

genericPIV_1_6 <- genericPIV_1 %>%
  mutate(ncompetitor = 5,
         t0 = 0,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(exclusivity)), as.numeric(as.Date("2012-09-30") - as.Date(exclusivity))),
         gaptime = t1,
         entry2 = 0)

genericPIV_1_5 <- genericPIV_1 %>%
  mutate(ncompetitor = 4,
         t0 = 0,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(exclusivity)), as.numeric(as.Date("2012-09-30") - as.Date(exclusivity))),
         gaptime = t1,
         entry2 = 0)

genericPIV_1_4 <- genericPIV_1 %>%
  mutate(ncompetitor = 3,
         t0 = 0,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(exclusivity)), as.numeric(as.Date("2012-09-30") - as.Date(exclusivity))),
         gaptime = t1,
         entry2 = 0)

genericPIV_1_3 <- genericPIV_1 %>%
  mutate(ncompetitor = 2,
         t0 = 0,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(exclusivity)), as.numeric(as.Date("2012-09-30") - as.Date(exclusivity))),
         gaptime = t1,
         entry2 = 0)

genericPIV_1_2 <- genericPIV_1 %>%
  mutate(ncompetitor = 1,
         t0 = 0,
         t1 = ifelse(GDUFA == 1, as.numeric(as.Date("2017-09-30") - as.Date(exclusivity)), as.numeric(as.Date("2012-09-30") - as.Date(exclusivity))),
         gaptime = t1,
         entry2 = 0)

#also add first entrants with exclusivity <= 2012-10-01, but have second generic entry after this
genericPIV_first_preGDUFA <- genericPIV_origin %>%
  filter(exclusivity < '2012-10-01' & date >= '2012-10-01' & order == 2)
genericPIV_first_preGDUFA$entry2 <- 0
genericPIV_first_preGDUFA$t0 <- 0
genericPIV_first_preGDUFA$gaptime <- genericPIV_first_preGDUFA$t1 <- as.numeric(as.Date("2012-09-30") - as.Date(genericPIV_first_preGDUFA$exclusivity))
genericPIV_first_preGDUFA$GDUFA <- 1
genericPIV_first_preGDUFA$indexyear <- round(as.integer(format(as.Date(genericPIV_first_preGDUFA$exclusivity), "%Y%m%d"))/10000)


genericPIV_first_preGDUFA_6 <- genericPIV_first_preGDUFA %>%
  mutate(ncompetitor = 5)
genericPIV_first_preGDUFA_5 <- genericPIV_first_preGDUFA %>%
  mutate(ncompetitor = 4)
genericPIV_first_preGDUFA_4 <- genericPIV_first_preGDUFA %>%
  mutate(ncompetitor = 3)
genericPIV_first_preGDUFA_3 <- genericPIV_first_preGDUFA %>%
  mutate(ncompetitor = 2)
genericPIV_first_preGDUFA_2 <- genericPIV_first_preGDUFA %>%
  mutate(ncompetitor = 1)


genericPIV_WLW <- genericPIV %>%
  rbind(genericPIV_1_2) %>%
  rbind(genericPIV_1_3) %>%
  rbind(genericPIV_1_4) %>%
  rbind(genericPIV_1_5) %>%
  rbind(genericPIV_1_6) %>%
  rbind(genericPIV_2_3) %>%
  rbind(genericPIV_2_4) %>%
  rbind(genericPIV_2_5) %>% 
  rbind(genericPIV_2_6) %>% 
  rbind(genericPIV_3_4) %>%
  rbind(genericPIV_3_5) %>% 
  rbind(genericPIV_3_6) %>%   
  rbind(genericPIV_4_5) %>% 
  rbind(genericPIV_4_6) %>% 
  rbind(genericPIV_5_6) %>%
  rbind(genericPIV_first_preGDUFA_2) %>%
  rbind(genericPIV_first_preGDUFA_3) %>%
  rbind(genericPIV_first_preGDUFA_4) %>%
  rbind(genericPIV_first_preGDUFA_5) %>%
  rbind(genericPIV_first_preGDUFA_6) 
  
genericPIV_WLW$t1[genericPIV_WLW$t0 == genericPIV_WLW$t1] <- as.numeric(genericPIV_WLW$t1[genericPIV_WLW$t0 == genericPIV_WLW$t1]) + 0.001


model_PIV_WLW <- coxph(Surv(t0, t1, entry2) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV  + cluster(index), method = "breslow", data = genericPIV_WLW)
model_PIV_WLW <- coxph(Surv(t1, entry2) ~ GDUFA*strata(ncompetitor) + route + AG + guidance_before + indexyear + ATCA + ATCB + ATCC + ATCD + ATCG + ATCH + ATCG +ATCL + ATCM + ATCN + ATCP + ATCR + ATCS + ATCV  + cluster(index), method = "breslow", robust = TRUE, data = genericPIV_WLW)
summary(model_PIV_WLW)
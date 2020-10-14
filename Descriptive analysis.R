##1. Count model (by year)
#build dataset 
#First exclude branded without any generic competition
genericnoPIV_nocensor <- genericnoPIV_origin %>% select(-approvemonth)
genericPIV_nocensor <- genericPIV_origin 

branded_count <- rbind(genericPIV_nocensor, genericnoPIV_nocensor)

branded_count <- branded_count %>% group_by(index) %>% arrange(desc(numdate)) 

branded_count <- branded_count %>% filter(row_number(numdate) == 1)

check <- branded_count %>% filter(count == 1)


#branded_count <- generic %>% filter(row_number(numdate) == 1)

table(branded_count$count)

test <- branded_count %>% filter(count==1)

#branded_count <- branded_count %>% filter(count > 1)


#Number of competitors
#branded_count$count <- branded_count$count - 1

mean(branded_count$count) #6.75
hist(branded_count$count)
var(branded_count$count) #23, not following poisson

#GDUFA variable different: now based on index year 
branded_count$GDUFA[branded_count$indexyear < 2013] <- 0
branded_count$GDUFA[branded_count$indexyear >= 2013] <- 1

#Follow-up time
branded_count$followup <- ifelse(branded_count$GDUFA == 0, 5, 2017 - branded_count$indexyear)
table(branded_count$followup)

#Whether any guidance 
branded_count$anyguidance <- ifelse(branded_count$min < branded_count$date_guidance, 1, 0)
table(branded_count$anyguidance)
branded_count$anyguidance[is.na(branded_count$anyguidance)] <- 0

#Limit to index year after 2007
branded_count <- branded_count %>% filter(indexyear >= 2007)

write.csv(branded_count, 'branded_count.csv')
#analysis conducted in STATA


###Falsification test
#Number of branded drug entry over time 
branded_all$year <- floor(branded_all$numdate/10000)

ggplot(branded_all, aes(x=year)) + geom_bar()

branded_all <- branded_all %>% filter(numdate >= 20070101 & numdate <= 20171231)
ggplot(branded_all, aes(x=year)) + geom_bar()

branded_all$generic <- 0

generic_all$year <- floor(generic_all$numdate/10000)
generic_all <- generic_all %>% filter(numdate >= 20070101 & numdate <= 20171230)
generic_all$generic <- 1


all <- rbind(branded_all, generic_all)

all$generic <- as.factor(all$generic)

ggplot(all, aes(x=year, fill = generic)) + 
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#F8766D", "#00BFC4"), name = "Type", labels = c("Branded", "Generic")) +
  ylab("Number")

figure2_df1 <- all %>%
  select(year, generic)

write.xlsx(figure2_df1, "figure2_year of approval.xlsx")

##2. Count model (by month, order of entry)

##0.1 With PIV
#Each year:# of generic order
byyear_PIV<-genericPIV %>%  group_by(approveyear) %>% dplyr::summarize(number = n())

#First generic only
genericPIV_1<-genericPIV[genericPIV[, "order"]==1, ]
byyear_PIV_1<-genericPIV_1 %>%  group_by(approveyear) %>% dplyr::summarize(number = n())

#Second generic only
genericPIV_2<-genericPIV[genericPIV[, "order"]==2, ]
byyear_PIV_2<-genericPIV_2 %>%  group_by(approveyear) %>% dplyr::summarize(number = n())

#Third generic only
genericPIV_3<-genericPIV[genericPIV[, "order"]==3, ]
byyear_PIV_3<-genericPIV_3 %>%  group_by(approveyear) %>% dplyr::summarize(number = n())

##0.1 Without PIV
#Each year:# of generic order
byyear_noPIV<-genericnoPIV %>%  group_by(approveyear) %>% dplyr::summarize(number = n())

#First generic only
genericnoPIV_1<-genericnoPIV[genericnoPIV[, "order"]==1, ]
byyear_noPIV_1<-genericnoPIV_1 %>%  group_by(approveyear) %>% dplyr::summarize(number = n())

#Second generic only
genericnoPIV_2<-genericnoPIV[genericnoPIV[, "order"]==2, ]
byyear_noPIV_2<-genericnoPIV_2 %>%  group_by(approveyear) %>% dplyr::summarize(number = n())

#Third generic only
genericnoPIV_3<-genericnoPIV[genericnoPIV[, "order"]==3, ]
byyear_noPIV_3<-genericnoPIV_3 %>%  group_by(approveyear) %>% dplyr::summarize(number = n())

###Approval month:
##With PIV
bymonth_PIV<-genericPIV_nocensor %>%  group_by(approvemonth) %>% dplyr::summarize(number = n())
write.csv(bymonth_PIV,'bymonth_PIV_total.csv')

#By month
bymonth_PIV_order<-genericPIV_nocensor %>%  group_by(.dots=c("order", "approvemonth")) %>% dplyr::summarize(number = n())
write.csv(bymonth_PIV_order,'bymonth_PIV_order.csv')

genericPIV_nocensor$order[genericPIV_nocensor$order >= 4] <- 4
genericPIV_nocensor$approvemonth <- as.Date(cut(genericPIV_nocensor$date, breaks = "month"))

bymonth_PIV_order<-genericPIV_nocensor %>%  group_by(.dots=c("order", "approvemonth")) %>% dplyr::summarize(number = n())
bymonth_PIV_order$order <- as.factor(bymonth_PIV_order$order)


dtLimits <- as.Date(c("2007-10-01", "2017-09-30"))

#p <- ggplot(genericPIV_nocensor, aes(date, ..count..)) + 
#  geom_histogram(aes(color = order), position = "stack") +
#  theme_bw() + xlab(NULL) +
#  scale_x_datetime(breaks = date_breaks("3 months"),
#                   labels = date_format("%Y-%b"),
#                   limits = c(as.POSIXct("2007-10-01"), 
#                             as.POSIXct("2017-09-30")) )

ggplot(bymonth_PIV_order) +
  geom_bar(aes(x= approvemonth, y = number, fill=order), stat="identity") +
  scale_x_date(date_breaks = "1 month", 
               labels = date_format("%y/%m"),
               limits = dtLimits)  +
  theme(axis.text.x = element_text(angle = 50, vjust = .5, size = 5)) +
  scale_fill_manual(values=wes_palette(n=4, name="GrandBudapest2"), name = "Order", labels = c("1", "2", "3", "4+")) +
  xlab("Month of Approval") +
  ylab("Number") 

ggplot(bymonth_PIV_order) +
  geom_bar(aes(x= approvemonth, y = number, fill=order), stat="identity") +
  scale_x_date(date_breaks = "3 month", 
               labels = date_format("%y/%m"),
               limits = dtLimits)  +
  theme(axis.text.x = element_text(angle = 50, vjust = .5, size = 10)) +
  scale_fill_manual(values=wes_palette(n=4, name="GrandBudapest2"), name = "Order", labels = c("1", "2", "3", "4+")) +
  xlab("Month of Approval") +
  ylab("Number") +
  geom_vline(xintercept = as.Date("2012-09-15"))

test <- bymonth_PIV_order %>% filter(order == 1)  

##Without PIV
bymonth_noPIV<-genericnoPIV_nocensor %>%  group_by(approvemonth) %>% dplyr::summarize(number = n())
write.csv(bymonth_noPIV,'bymonth_noPIV_total.csv')

#By month
bymonth_noPIV_order<-genericnoPIV_nocensor %>%  group_by(.dots=c("order", "approvemonth")) %>% dplyr::summarize(number = n())
write.csv(bymonth_noPIV_order,'bymonth_noPIV_order.csv')

genericnoPIV_nocensor$order[genericnoPIV_nocensor$order >= 4] <- 4
genericnoPIV_nocensor$approvemonth <- as.Date(cut(genericnoPIV_nocensor$date, breaks = "month"))

bymonth_noPIV_order<-genericnoPIV_nocensor %>%  group_by(.dots=c("order", "approvemonth")) %>% dplyr::summarize(number = n())
bymonth_noPIV_order$order <- as.factor(bymonth_noPIV_order$order)

ggplot(bymonth_noPIV_order) +
  geom_bar(aes(x= approvemonth, y = number, fill=order), stat="identity") +
  scale_x_date(date_breaks = "3 month", 
               labels = date_format("%y/%m"),
               limits = dtLimits)  +
  theme(axis.text.x = element_text(angle = 50, vjust = .5, size = 10)) +
  scale_fill_manual(values=wes_palette(n=4, name="GrandBudapest2"), name = "Order", labels = c("1", "2", "3", "4+")) +
  xlab("Month of Approval") +
  ylab("Number") +
  geom_vline(xintercept = as.Date("2012-09-15"))

firstnoPIV <- genericnoPIV_nocensor %>% filter(order == 1)
firstnoPIV$minmonth <- as.Date(cut(firstnoPIV$min, breaks = "month"))
firstnoPIV$maxmonth <- as.Date(cut(firstnoPIV$max, breaks = "month"))

firstnoPIV_0717_min <- firstnoPIV %>% filter(min >= "2007-10-01")
firstnoPIV_0717_min %>% group_by(min >= "2012-10-01") %>% tally()
firstnoPIV_0717_max <- firstnoPIV %>% filter(max >= "2007-10-01")
firstnoPIV_0717_max %>% group_by(max >= "2012-10-01") %>% tally()

firstnoPIV$minyear<- as.Date(cut(firstnoPIV$min, breaks = "year"))

bymonth_firstnoPIV<-firstnoPIV %>%  group_by(minmonth) %>% dplyr::summarize(number = n())
ggplot(bymonth_firstnoPIV) +
  geom_bar(aes(x= minmonth, y = number), stat="identity") +
  scale_x_date(date_breaks = "3 month", 
               labels = date_format("%y/%m"),
               limits = dtLimits)  +
  theme(axis.text.x = element_text(angle = 50, vjust = .5, size = 10)) +
  scale_fill_manual(values=wes_palette(n=1, name="GrandBudapest2")) +
  xlab("Month of Earliest Patent Expiration") +
  ylab("Number") +
  geom_vline(xintercept = as.Date("2012-09-15"))

byyear_firstnoPIV<-firstnoPIV %>%  group_by(minyear) %>% dplyr::summarize(number = n())
ggplot(byyear_firstnoPIV) +
  geom_bar(aes(x= minyear, y = number), stat="identity") +
  scale_x_date(date_breaks = "1 year", 
               labels = date_format("%y"),
               limits = dtLimits)  +
  theme(axis.text.x = element_text(angle = 50, vjust = .5, size = 10)) +
  scale_fill_manual(values=wes_palette(n=1, name="GrandBudapest2")) +
  xlab("Year of Earliest Patent Expiration") +
  ylab("Number") +
  geom_vline(xintercept = as.Date("2012-01-01"))


bymonth_firstnoPIV_latest<-firstnoPIV %>%  group_by(maxmonth) %>% dplyr::summarize(number = n())
ggplot(bymonth_firstnoPIV_latest) +
  geom_bar(aes(x= maxmonth, y = number), stat="identity") +
  scale_x_date(date_breaks = "3 month", 
               labels = date_format("%y/%m"),
               limits = dtLimits)  +
  theme(axis.text.x = element_text(angle = 50, vjust = .5, size = 10)) +
  scale_fill_manual(values=wes_palette(n=1, name="GrandBudapest2")) +
  xlab("Month of Latest Patent Expiration") +
  ylab("Number") +
  geom_vline(xintercept = as.Date("2012-09-15"))

firstnoPIV$maxyear<- as.Date(cut(firstnoPIV$max, breaks = "year"))
byyear_firstnoPIV_latest<-firstnoPIV %>%  group_by(maxyear) %>% dplyr::summarize(number = n())

byyear_firstnoPIV_latest$type <- "Latest"
byyear_firstnoPIV$type <- "Earliest"
colnames(byyear_firstnoPIV)[1] <- "year"
colnames(byyear_firstnoPIV_latest)[1] <- "year"

byyear_firstnoPIV_both <- rbind(byyear_firstnoPIV, byyear_firstnoPIV_latest)
byyear_firstnoPIV_both <- byyear_firstnoPIV_both %>% filter(year >= "2007-01-01")
byyear_firstnoPIV_both <- byyear_firstnoPIV_both %>% filter(year <= "2017-12-30")

ggplot(byyear_firstnoPIV_both) +
  geom_bar(aes(x= year, y = number, fill = type), stat="identity", position="dodge2") +
  #scale_x_date(date_breaks = "1 year", 
  #             labels = date_format("%y"),
  #             limits = dtLimits)  +
  theme(axis.text.x = element_text(angle = 0, vjust = .5, size = 10)) +
  #scale_fill_manual(values=wes_palette(n=2, name="Moonrise3"), name="Type") +
  scale_fill_manual(values=c("#F8766D", "#00BFC4"), name="Type") +
  xlab("Year of Patent Expiration") +
  ylab("Number") 

figure2_df2 <- byyear_firstnoPIV_both %>%
  select(year, number, type)

write.xlsx(figure2_df2, "figure2_year of patent expiry.xlsx")

#no PIV
##first entrants only (plus censored with no first entry)
genericnoPIV_first <- genericnoPIV %>% filter(ncompetitor == 0)

model_noPIV_cox_first_unadj <- coxph(Surv(gaptime, entry1) ~ GDUFA, data = genericnoPIV_first)
summary(model_noPIV_cox_first_unadj)

model_noPIV_cox_first_unadj <- survfit(Surv(gaptime, entry1) ~ GDUFA, data = genericnoPIV_first)
summary(model_noPIV_cox_first_unadj)
ggsurvplot(model_noPIV_cox_first_unadj,
           #risk.table = TRUE,       # show risk table.
           #pval = TRUE,             # show p-value of log-rank test.
           conf.int = TRUE,         # show confidence intervals for 
           # point estimates of survival curves.
           xlim = c(0, 1500),         # present narrower X axis, but not affect
           palette = c("grey50", "black"),
           # survival estimates.
           xlab = "Time to first generic entry",   # customize X axis label.
           break.time.by = 500,     # break X axis in time intervals by 500.
           ggtheme = theme_light(), # customize plot and risk table with a theme.
           risk.table.y.text.col = T, # colour risk table text annotations.
           risk.table.y.text = FALSE, # show bars instead of names in text annotations
           # in legend of risk table
           legend.labs = c("Pre-GDUFA", "Post-GDUFA")
)

figure3_df3 <- genericnoPIV_first %>%
  select(gaptime, entry1, GDUFA) %>%
  filter(!is.na(entry1))

write.xlsx(figure3_df3, "figure3_noPC_time_to_first_entry.xlsx")


model_noPIV_cox_first <- coxph(Surv(gaptime, entry1) ~ GDUFA + route + AG + ATC1 + ETASU + guidance_before + indexyear, method = "breslow", data = genericnoPIV_first)
summary(model_noPIV_cox_first)

##second entrants only (plus censored with no second entry)
genericnoPIV_second <- genericnoPIV %>% 
  filter(order == 1 | order == 2,
         entry1 == 1) %>% 
  select(-count_generic)

#add previous failure time (first entrant)
#genericnoPIV_second_previousfailure <- genericnoPIV_second %>% filter(order == 1) %>% select(index, lag)
#colnames(genericnoPIV_second_previousfailure)[which(names(genericnoPIV_second_previousfailure) == "lag")] <- "previousfailuret"
#exclude duplicates
#genericnoPIV_second_previousfailure <- genericnoPIV_second_previousfailure[!duplicated(genericnoPIV_second_previousfailure), ]

#genericnoPIV_second <- inner_join(genericnoPIV_second, genericnoPIV_second_previousfailure)

genericnoPIV_second_index <- genericnoPIV_second %>% 
  group_by(index) %>% 
  dplyr::summarise(count_generic=n()) 

genericnoPIV_first_index <- genericnoPIV_second %>% 
  filter(order == 1) %>%
  group_by(index) %>% 
  dplyr::summarise(count_generic_first=n()) 

genericnoPIV_second <- genericnoPIV_second %>%
  left_join(genericnoPIV_second_index, by = "index") %>%
  left_join(genericnoPIV_first_index, by = "index")

genericnoPIV_second <- genericnoPIV_second %>% filter((order == 1 & count_generic == count_generic_first) | order == 2)

genericnoPIV_second$entry2 <- ifelse(genericnoPIV_second$order ==2, 1, 0)

# recode survival time for censored first entrants
genericnoPIV_second_1_preGDUFA <- genericnoPIV_second %>% 
  filter(order == 1 & min < "2012-10-01") %>%
  mutate(t1 = as.numeric(as.Date("2012-09-30") - as.Date(min)),
         gaptime = as.numeric(as.Date("2012-09-30") - as.Date(date))) 

genericnoPIV_second_1_postGDUFA <- genericnoPIV_second %>% 
  filter(order == 1 & min >= "2012-10-01") %>%
  mutate(t1 = as.numeric(as.Date("2017-09-30") - as.Date(min)),
         gaptime = as.numeric(as.Date("2017-09-30") - as.Date(date))) 

genericnoPIV_second_1 <- genericnoPIV_second_1_preGDUFA %>%
  bind_rows(genericnoPIV_second_1_postGDUFA)

genericnoPIV_second <- genericnoPIV_second %>%
  filter(order == 2) %>%
  bind_rows(genericnoPIV_second_1)

#model_noPIV_cox_second <- coxph(Surv(gaptime, entry2) ~ GDUFA + route + AG + ATC1 + ETASU + guidance_before + previousfailuret + approveyear, method = "breslow", data = genericnoPIV_second)
#summary(model_noPIV_cox_second)

#TT
model_noPIV_cox_second_unadj <- survfit(Surv(t1, entry2) ~ GDUFA, data = genericnoPIV_second)
ggsurvplot(model_noPIV_cox_second_unadj,
           #risk.table = TRUE,       # show risk table.
           #pval = TRUE,             # show p-value of log-rank test.
           conf.int = TRUE,         # show confidence intervals for 
           # point estimates of survival curves.
           xlim = c(0, 3000),         # present narrower X axis, but not affect
           palette = c("grey50", "black"),
           # survival estimates.
           xlab = "Time to second generic entry",   # customize X axis label.
           break.time.by = 1500,     # break X axis in time intervals by 500.
           ggtheme = theme_light(), # customize plot and risk table with a theme.
           risk.table.y.text.col = T, # colour risk table text annotations.
           risk.table.y.text = FALSE, # show bars instead of names in text annotations
           # in legend of risk table
           legend.labs = c("Pre-GDUFA", "Post-GDUFA")
)
#GT
model_noPIV_cox_second_unadj <- survfit(Surv(gaptime, entry2) ~ GDUFA, data = genericnoPIV_second)
ggsurvplot(model_noPIV_cox_second_unadj,
           #risk.table = TRUE,       # show risk table.
           #pval = TRUE,             # show p-value of log-rank test.
           conf.int = TRUE,         # show confidence intervals for 
           # point estimates of survival curves.
           xlim = c(0, 1500),         # present narrower X axis, but not affect
           palette = c("grey50", "black"),
           # survival estimates.
           xlab = "Time to second generic entry",   # customize X axis label.
           break.time.by = 500,     # break X axis in time intervals by 500.
           ggtheme = theme_light(), # customize plot and risk table with a theme.
           risk.table.y.text.col = T, # colour risk table text annotations.
           risk.table.y.text = FALSE, # show bars instead of names in text annotations
           # in legend of risk table
           legend.labs = c("Pre-GDUFA", "Post-GDUFA")
)

figure3_df4 <- genericnoPIV_second %>%
  select(gaptime, entry2, GDUFA) %>%
  filter(!is.na(entry2))

write.xlsx(figure3_df4, "figure3_noPC_time_to_second_entry.xlsx")


##third entrants only (plus censored with no third entry)
genericnoPIV_third <- genericnoPIV %>% 
  filter(order == 2 | order == 3, 
         entry1 == 1) %>% 
  select(-count_generic) 

genericnoPIV_third_index <- genericnoPIV_third %>% 
  group_by(index) %>% 
  dplyr::summarise(count_generic=n()) 

genericnoPIV_third  <- genericnoPIV_third %>%
  left_join(genericnoPIV_third_index, by = "index") 

genericnoPIV_third <- genericnoPIV_third %>% filter((order == 2 & count_generic == 1) | order == 3)

genericnoPIV_third$entry3 <- ifelse(genericnoPIV_third$order == 3, 1, 0)

# recode survival time for censored second entrants
genericnoPIV_third_2_preGDUFA <- genericnoPIV_third %>% 
  filter(order == 2 & min < "2012-10-01") %>%
  mutate(t1 = as.numeric(as.Date("2012-09-30") - as.Date(min)),
         gaptime = as.numeric(as.Date("2012-09-30") - as.Date(date))) 

genericnoPIV_third_2_postGDUFA <- genericnoPIV_third %>% 
  filter(order == 2 & min >= "2012-10-01") %>%
  mutate(t1 = as.numeric(as.Date("2017-09-30") - as.Date(min)),
         gaptime = as.numeric(as.Date("2017-09-30") - as.Date(date))) 

genericnoPIV_third_2 <- genericnoPIV_third_2_preGDUFA %>%
  bind_rows(genericnoPIV_third_2_postGDUFA)

genericnoPIV_third <- genericnoPIV_third %>%
  filter(order == 3) %>%
  bind_rows(genericnoPIV_third_2)

#model_noPIV_cox_third <- coxph(Surv(gaptime, entry3) ~ GDUFA + route + AG + ATC1 + ETASU + guidance_before + previousfailuret + approveyear, method = "breslow", data = genericnoPIV_third)
#summary(model_noPIV_cox_third)

#TT
model_noPIV_cox_third_unadj <- survfit(coxph(Surv(t0, t1, entry3, type = "counting") ~ GDUFA, data = genericnoPIV_third))
model_noPIV_cox_third_unadj_res <- survfit(Surv(t0, t1, entry3, type = "counting") ~ GDUFA, data = genericnoPIV_third)
test <- Surv(genericnoPIV_third$t0, genericnoPIV_third$t1, genericnoPIV_third$entry3)
my.fit <- survfit(test~genericnoPIV_third$GDUFA)
survfit(model_noPIV_cox_third_unadj_res)
plot(model_noPIV_cox_third_unadj, xlim = c(0, 150))
ggsurvplot(model_noPIV_cox_third_unadj_res,
           #risk.table = TRUE,       # show risk table.
           #pval = TRUE,             # show p-value of log-rank test.
           conf.int = TRUE,         # show confidence intervals for 
           # point estimates of survival curves.
           xlim = c(0, 100),         # present narrower X axis, but not affect
           palette = c("grey50", "black"),
           # survival estimates.
           xlab = "Time to third generic entry",   # customize X axis label.
           break.time.by = 1500,     # break X axis in time intervals by 500.
           ggtheme = theme_light(), # customize plot and risk table with a theme.
           risk.table.y.text.col = T, # colour risk table text annotations.
           risk.table.y.text = FALSE, # show bars instead of names in text annotations
           # in legend of risk table
           legend.labs = c("Pre-GDUFA", "Post-GDUFA")
)

test <- genericnoPIV_third %>%
  survfit(Surv(t0, t1, entry3) ~ GDUFA, .) %>%
  tidy()

km <-
  ggplot(test, aes(x = time, y = estimate, col = strata)) +
  geom_step() +
  theme_bw() +
  #xlim(0, 24) +
  xlab("Time in days") +
  ylab("% entered")



##GT
model_noPIV_cox_third_unadj <- survfit(Surv(gaptime, entry3) ~ GDUFA, data = genericnoPIV_third)
ggsurvplot(model_noPIV_cox_third_unadj,
           #risk.table = TRUE,       # show risk table.
           #pval = TRUE,             # show p-value of log-rank test.
           conf.int = TRUE,         # show confidence intervals for 
           # point estimates of survival curves.
           xlim = c(0, 1500),         # present narrower X axis, but not affect
           palette = c("grey50", "black"),
           # survival estimates.
           xlab = "Time to third generic entry",   # customize X axis label.
           break.time.by = 500,     # break X axis in time intervals by 500.
           ggtheme = theme_light(), # customize plot and risk table with a theme.
           risk.table.y.text.col = T, # colour risk table text annotations.
           risk.table.y.text = FALSE, # show bars instead of names in text annotations
           # in legend of risk table
           legend.labs = c("Pre-GDUFA", "Post-GDUFA")
)

figure3_df5 <- genericnoPIV_third %>%
  select(gaptime, entry3, GDUFA) %>%
  filter(!is.na(entry3))

write.xlsx(figure3_df5, "figure3_noPC_time_to_third_entry.xlsx")

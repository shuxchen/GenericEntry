##PIV
##second only
genericPIV_second <- genericPIV %>% filter(ncompetitor == 1)

model_PIV_cox_second_unadj <- survfit(Surv(gaptime, entry2) ~ GDUFA, data = genericPIV_second)
ggsurvplot(model_PIV_cox_second_unadj,
           #risk.table = TRUE,       # show risk table.
           #pval = TRUE,             # show p-value of log-rank test.
           conf.int = TRUE,         # show confidence intervals for 
           # point estimates of survival curves.
           xlim = c(0,3000),         # present narrower X axis, but not affect
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


##third entrants only (plus censored with no third entry)
genericPIV_third <- genericPIV %>% 
  filter(order == 2 | order == 3,
         entry2 == 1) %>% 
  select(-count_generic)

genericPIV_third_index <- genericPIV_third %>% 
  group_by(index) %>% 
  dplyr::summarise(count_generic=n()) 
genericPIV_second_index <- genericPIV_third %>% 
  filter(order == 2) %>% 
  group_by(index) %>% 
  dplyr::summarise(count_generic_second=n()) 

genericPIV_third  <- genericPIV_third %>%
  left_join(genericPIV_third_index, by = "index") %>% 
  left_join(genericPIV_second_index, by = "index")

genericPIV_third <- genericPIV_third %>% filter((order == 2 & count_generic == count_generic_second) | order == 3)

genericPIV_third$entry3 <- ifelse(genericPIV_third$order == 3, 1, 0)

# recode survival time for censored second entrants
genericPIV_third_2_preGDUFA <- genericPIV_third %>% 
  filter(order == 2 & exclusivity < "2012-10-01") %>%
  mutate(t1 = as.numeric(as.Date("2012-09-30") - as.Date(exclusivity)),
         gaptime = as.numeric(as.Date("2012-09-30") - as.Date(date))) 

genericPIV_third_2_postGDUFA <- genericPIV_third %>% 
  filter(order == 2 & exclusivity >= "2012-10-01") %>%
  mutate(t1 = as.numeric(as.Date("2017-09-30") - as.Date(exclusivity)),
         gaptime = as.numeric(as.Date("2017-09-30") - as.Date(date))) 

genericPIV_third_2 <- genericPIV_third_2_preGDUFA %>%
  bind_rows(genericPIV_third_2_postGDUFA)

genericPIV_third <- genericPIV_third %>%
  filter(order == 3) %>%
  bind_rows(genericPIV_third_2)

#TT
model_PIV_cox_third_unadj <- survfit(Surv(t1, entry3) ~ GDUFA, data = genericPIV_third)
ggsurvplot(model_PIV_cox_third_unadj,
           #risk.table = TRUE,       # show risk table.
           #pval = TRUE,             # show p-value of log-rank test.
           conf.int = TRUE,         # show confidence intervals for 
           # point estimates of survival curves.
           xlim = c(0, 3000),         # present narrower X axis, but not affect
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

#GT
model_PIV_cox_third_unadj <- survfit(Surv(lag, entry3) ~ GDUFA, data = genericPIV_third)
ggsurvplot(model_PIV_cox_third_unadj,
           #risk.table = TRUE,       # show risk table.
           #pval = TRUE,             # show p-value of log-rank test.
           conf.int = TRUE,         # show confidence intervals for 
           # point estimates of survival curves.
           xlim = c(0, 3000),         # present narrower X axis, but not affect
           palette = c("grey50", "black"),
           # survival estimates.
           xlab = "Time to third generic entry (GT)",   # customize X axis label.
           break.time.by = 1000,     # break X axis in time intervals by 500.
           ggtheme = theme_light(), # customize plot and risk table with a theme.
           risk.table.y.text.col = T, # colour risk table text annotations.
           risk.table.y.text = FALSE, # show bars instead of names in text annotations
           # in legend of risk table
           legend.labs = c("Pre-GDUFA", "Post-GDUFA")
)

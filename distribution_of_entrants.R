## Distribution of entrants 
# In 2012 
# Index date in 2012; filter approval in 2012

# load data
load("genericPIV.RData")
load("genericnoPIV.RData")
load("all.RData")

distribution_of_generics_PC <- function(df, year){
  df_year <- df %>%
    filter(year(exclusivity) == year) %>%
    filter(approveyear == year) %>%
    group_by(index) %>%
    arrange(desc(order)) %>%
    top_n(1) %>% 
    group_by(order) %>%
    summarise(count = n()) %>%
    mutate(p = count/sum(count)) 
  
  df_year_4 <- df_year %>% 
    filter(order >= 4) %>%
    summarise(count = sum(count),
              p = sum(p)) %>% 
    mutate(order = 4) %>%
    arrange(order, count, p)
  
  data <- df_year %>%
    filter(order < 4) %>%
    rbind(df_year_4)
  
  return(data)
}

dist_PC_2011 <- distribution_of_generics_PC(df = genericPIV_origin, year = 2011)
dist_PC_2010 <- distribution_of_generics_PC(df = genericPIV_origin, year = 2010)
dist_PC_2009 <- distribution_of_generics_PC(df = genericPIV_origin, year = 2009)
dist_PC_2008 <- distribution_of_generics_PC(df = genericPIV_origin, year = 2008)


distribution_of_generics_noPC <- function(df, year){
  df_year <- df %>%
    filter(year(min) == year) %>%
    filter(approveyear == year) %>%
    group_by(index) %>%
    arrange(desc(order)) %>%
    top_n(1) %>% 
    group_by(order) %>%
    summarise(count = n()) %>%
    mutate(p = count/sum(count)) 
  
  df_year_4 <- df_year %>% 
    filter(order >= 4) %>%
    summarise(count = sum(count),
              p = sum(p)) %>% 
    mutate(order = 4) %>%
    arrange(order, count, p)
  
  data <- df_year %>%
    filter(order < 4) %>%
    rbind(df_year_4)
  
  return(data)
} 

dist_noPC_2011 <- distribution_of_generics_noPC(df = genericnoPIV_origin, year = 2011)
dist_noPC_2010 <- distribution_of_generics_noPC(df = genericnoPIV_origin, year = 2010)
dist_noPC_2009 <- distribution_of_generics_noPC(df = genericnoPIV_origin, year = 2009)
dist_noPC_2008 <- distribution_of_generics_noPC(df = genericnoPIV_origin, year = 2008)

distribution_of_generics <- function(df_PC, df_noPC, year){
  df_PC_year <- df_PC %>%
    filter(year(exclusivity) == year) %>%
    filter(approveyear == year) %>%
    group_by(index) %>%
    arrange(desc(order)) %>%
    top_n(1) %>% 
    group_by(order) %>%
    summarise(count = n()) 
  
  df_PC_year_4 <- df_PC_year %>% 
    filter(order >= 4) %>%
    summarise(count = sum(count)) %>% 
    mutate(order = 4) %>%
    arrange(order, count)
  
  PC <- df_PC_year %>%
    filter(order < 4) %>%
    rbind(df_PC_year_4)
  
  df_noPC_year <- df_noPC %>%
    filter(year(min) == year) %>%
    filter(approveyear == year) %>%
    group_by(index) %>%
    arrange(desc(order)) %>%
    top_n(1) %>% 
    group_by(order) %>%
    summarise(count = n()) 
  
  df_noPC_year_4 <- df_noPC_year %>% 
    filter(order >= 4) %>%
    summarise(count = sum(count)) %>% 
    mutate(order = 4) %>%
    arrange(order, count)
  
  noPC <- df_noPC_year %>%
    filter(order < 4) %>%
    rbind(df_noPC_year_4)
  
  data <- PC %>%
    rbind(noPC) %>%
    group_by(order) %>%
    summarise(count = sum(count)) %>%
    mutate(p = count/sum(count)) 
  
  return(data)
} 

dist_all_2011 <- distribution_of_generics(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2011) %>%
  mutate(year = 2011)
dist_all_2010 <- distribution_of_generics(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2010) %>%
  mutate(year = 2010)
dist_all_2009 <- distribution_of_generics(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2009) %>%
  mutate(year = 2009)
dist_all_2008 <- distribution_of_generics(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2008) %>%
  mutate(year = 2008)
dist_all_2012 <- distribution_of_generics(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2012) %>%
  mutate(year = 2012)
dist_all_2013 <- distribution_of_generics(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2013) %>%
  mutate(year = 2013)
dist_all_2014 <- distribution_of_generics(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2014) %>%
  mutate(year = 2014)
dist_all_2015 <- distribution_of_generics(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2015) %>%
  mutate(year = 2015)
dist_all_2016 <- distribution_of_generics(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2016) %>%
  mutate(year = 2016)

dist_all <- dist_all_2008 %>%
  rbind(dist_all_2009) %>% 
  rbind(dist_all_2010) %>%
  rbind(dist_all_2011) %>%
  rbind(dist_all_2012) %>%
  rbind(dist_all_2013) %>%
  rbind(dist_all_2014) %>%
  rbind(dist_all_2015) %>%
  rbind(dist_all_2016)  
  
ggplot(dist_all, aes(x=order, y=p, colour=as.factor(year))) + 
  geom_line() + 
  geom_point(size=2.5)

dist_all_mean <- dist_all %>%
  group_by(order) %>%
  summarise(mean = mean(p),
            sd = sd(p))

dist_all_mean_before2012 <- dist_all %>%
  filter(year < 2012) %>%
  group_by(order) %>%
  summarise(mean = mean(p),
            sd = sd(p))



# look beyond 4's entrant 
distribution_of_generics_all <- function(df_PC, df_noPC, year){
  PC <- df_PC %>%
    filter(year(exclusivity) == year) %>%
    filter(approveyear == year) %>%
    group_by(index) %>%
    arrange(desc(order)) %>%
    top_n(1) %>% 
    group_by(order) %>%
    summarise(count = n()) 
  
  noPC <- df_noPC %>%
    filter(year(min) == year) %>%
    filter(approveyear == year) %>%
    group_by(index) %>%
    arrange(desc(order)) %>%
    top_n(1) %>% 
    group_by(order) %>%
    summarise(count = n()) 
  
  data <- PC %>%
    rbind(noPC) %>%
    group_by(order) %>%
    summarise(count = sum(count)) %>%
    mutate(p = count/sum(count)) 
  
  return(data)
} 

dist_all_2011 <- distribution_of_generics_all(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2011) %>%
  mutate(year = 2011)
dist_all_2010 <- distribution_of_generics_all(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2010) %>%
  mutate(year = 2010)
dist_all_2009 <- distribution_of_generics_all(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2009) %>%
  mutate(year = 2009)
dist_all_2008 <- distribution_of_generics_all(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2008) %>%
  mutate(year = 2008)
dist_all_2012 <- distribution_of_generics_all(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2012) %>%
  mutate(year = 2012)
dist_all_2013 <- distribution_of_generics_all(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2013) %>%
  mutate(year = 2013)
dist_all_2014 <- distribution_of_generics_all(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2014) %>%
  mutate(year = 2014)
dist_all_2015 <- distribution_of_generics_all(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2015) %>%
  mutate(year = 2015)
dist_all_2016 <- distribution_of_generics_all(df_PC = genericPIV_origin, 
                                          df_noPC = genericnoPIV_origin,
                                          year = 2016) %>%
  mutate(year = 2016)

dist_all <- dist_all_2008 %>%
  rbind(dist_all_2009) %>% 
  rbind(dist_all_2010) %>%
  rbind(dist_all_2011) %>%
  rbind(dist_all_2012) %>%
  rbind(dist_all_2013) %>%
  rbind(dist_all_2014) %>%
  rbind(dist_all_2015) %>%
  rbind(dist_all_2016)  

dist_all_before2012 <- dist_all %>%
  filter(year < 2012)

ggplot(dist_all, aes(x=order, y=p, colour=as.factor(year))) + 
  geom_line() + 
  geom_point(size=2.5)

ggplot(dist_all_before2012, aes(x=order, y=p, colour=as.factor(year))) + 
  geom_line() + 
  geom_point(size=2.5)

dist_all_mean <- dist_all %>%
  group_by(order) %>%
  summarise(mean = mean(p),
            sd = sd(p))

dist_all_mean_before2012 <- dist_all %>%
  filter(year < 2012) %>%
  group_by(order) %>%
  summarise(mean = mean(p),
            sd = sd(p))


## calculate all # of entrants
# with PC
genericPIV_2011 <- genericPIV_origin %>%
  filter(year(exclusivity) == 2011) %>%
  filter(approveyear == 2011)

mean(genericPIV_2011$order)
var(genericPIV_2011$order)

genericPIV_2010 <- genericPIV_origin %>%
  filter(year(exclusivity) == 2010) %>%
  filter(approveyear == 2010)

mean(genericPIV_2010$order)
var(genericPIV_2010$order)

genericPIV_2009 <- genericPIV_origin %>%
  filter(year(exclusivity) == 2009) %>%
  filter(approveyear == 2009)

genericPIV_2008 <- genericPIV_origin %>%
  filter(year(exclusivity) == 2008) %>%
  filter(approveyear == 2008)

genericPIV_2012 <- genericPIV_origin %>%
  filter(year(exclusivity) == 2012) %>%
  filter(approveyear == 2012)

genericPIV_2013 <- genericPIV_origin %>%
  filter(year(exclusivity) == 2013) %>%
  filter(approveyear == 2013)

genericPIV_before2012 <- genericPIV_2008 %>%
  bind_rows(genericPIV_2009) %>%
  bind_rows(genericPIV_2010) %>%
  bind_rows(genericPIV_2011)

mean(genericPIV_before2012$order)
var(genericPIV_before2012$order)

genericPIV_before2012_count <- genericPIV_before2012 %>%
  group_by(order) %>%
  summarise(count = n()) %>%
  mutate(p = count/sum(count)) 

ggplot(genericPIV_before2012_count, aes(x=order, y=count)) + 
  geom_bar(stat="identity")

# no PC
genericnoPIV_2011 <- genericnoPIV_origin %>%
  filter(year(min) == 2011) %>%
  filter(approveyear == 2011)

mean(genericnoPIV_2011$order)
var(genericnoPIV_2011$order)

genericnoPIV_2010 <- genericnoPIV_origin %>%
  filter(year(min) == 2010) %>%
  filter(approveyear == 2010)

mean(genericnoPIV_2010$order)
var(genericnoPIV_2010$order)

genericnoPIV_2009 <- genericnoPIV_origin %>%
  filter(year(min) == 2009) %>%
  filter(approveyear == 2009)
genericnoPIV_2008 <- genericnoPIV_origin %>%
  filter(year(min) == 2008) %>%
  filter(approveyear == 2008)

genericnoPIV_2012 <- genericnoPIV_origin %>%
  filter(year(min) == 2012) %>%
  filter(approveyear == 2012)

genericnoPIV_2013 <- genericnoPIV_origin %>%
  filter(year(min) == 2013) %>%
  filter(approveyear == 2013)
genericnoPIV_before2012 <- genericnoPIV_2008 %>%
  bind_rows(genericnoPIV_2009) %>%
  bind_rows(genericnoPIV_2010) %>%
  bind_rows(genericnoPIV_2011)

mean(genericnoPIV_before2012$order)
var(genericnoPIV_before2012$order)

genericnoPIV_before2012_count <- genericnoPIV_before2012 %>%
  group_by(order) %>%
  summarise(count = n()) %>%
  mutate(p = count/sum(count)) 

ggplot(genericnoPIV_before2012_count, aes(x=order, y=count)) + 
  geom_bar(stat="identity")

# branded
#when first entry entered not in the same year 

get_branded_count <- function(df, year){
  total <- df %>%
    filter(year(min) == year) %>%
    filter(year(date) == year) 
  
  total_branded <- total %>%
    group_by(index) %>%
    summarise(count = n()) %>%
    filter(count == 1) %>%
    count() %>%
    pull()
  
  return(total_branded)
}

get_branded_index <- function(df, year){
  total <- df %>%
    filter(year(min) == year) %>%
    filter(year(date) == year) 
  
  total_branded <- total %>%
    group_by(index) %>%
    summarise(count = n()) %>%
    filter(count == 1) %>%
    mutate(order = 0) %>%
    dplyr::select(-count)
  
  return(total_branded)
}

total_2011_branded <- get_branded_count(df, 2011)
total_2012_branded <- get_branded_count(df, 2012)
total_2010_branded <- get_branded_count(df, 2010)
total_2009_branded <- get_branded_count(df, 2009)
total_2008_branded <- get_branded_count(df, 2008)
total_2012_branded <- get_branded_count(df, 2012)
total_2013_branded <- get_branded_count(df, 2013)


total_2011_branded_index <- get_branded_index(df, 2011)
total_2010_branded_index <- get_branded_index(df, 2010)
total_2009_branded_index <- get_branded_index(df, 2009)
total_2008_branded_index <- get_branded_index(df, 2008)
total_2012_branded_index <- get_branded_index(df, 2012)
total_2013_branded_index <- get_branded_index(df, 2013)


total_branded_before2012 <- total_2011_branded + total_2010_branded + total_2009_branded + total_2008_branded

total_branded_index_before2012 <- total_2011_branded_index %>%
  rbind(total_2010_branded_index) %>%
  rbind(total_2009_branded_index) %>%
  rbind(total_2008_branded_index) 

branded_nogeneric <- df %>% filter(Appl_Type=="N" & count == 1)

total_branded <- total_branded_index_before2012 %>%
  left_join(branded_nogeneric, on = 'index') %>%
  dplyr::select(index) %>%
  mutate(order = as.integer(0))
  
# comine branded, no PC and PC
total_before2012_count <- genericPIV_before2012_count %>%
  rbind(genericnoPIV_before2012_count) %>%
  group_by(order) %>%
  summarise(count = sum(count)) %>%
  add_row(order = 0, count = total_branded_before2012) %>%
  arrange(order) %>%
  mutate(p = count/sum(count))

total_before2012_all <- genericnoPIV_before2012 %>%
  bind_rows(genericPIV_before2012) %>%
  dplyr::select(index, order) %>%
  dplyr::bind_rows(total_branded) 


mean(total_before2012_all$order)
var(total_before2012_all$order)

ggplot(total_before2012_count, aes(x=order, y=count)) + 
  geom_bar(stat="identity")

# distribution each year
total_count_year <- function(noPC, PC, branded_count){
  total_count <- noPC %>%
    rbind(PC) %>%
    group_by(order) %>%
    summarise(count = n()) %>%
    add_row(order = 0, count = branded_count) %>%
    arrange(order) %>%
    mutate(p = count/sum(count))
  
  return(total_count)
}

total_all_year <- function(noPC, PC, branded){
  total_all <- noPC %>%
    bind_rows(PC) %>%
    dplyr::select(index, order) %>%
    dplyr::bind_rows(branded) 
  
  return(total_all)
}


total_2011_count <- total_count_year(noPC = genericnoPIV_2011, PC = genericPIV_2011, branded_count = total_2011_branded)
total_2010_count <- total_count_year(noPC = genericnoPIV_2010, PC = genericPIV_2010, branded_count = total_2010_branded)
total_2009_count <- total_count_year(noPC = genericnoPIV_2009, PC = genericPIV_2009, branded_count = total_2009_branded)
total_2008_count <- total_count_year(noPC = genericnoPIV_2008, PC = genericPIV_2008, branded_count = total_2008_branded)
total_2012_count <- total_count_year(noPC = genericnoPIV_2012, PC = genericPIV_2012, branded_count = total_2012_branded)
total_2013_count <- total_count_year(noPC = genericnoPIV_2013, PC = genericPIV_2013, branded_count = total_2013_branded)

total_2011_all <- total_all_year(noPC = genericnoPIV_2011, PC = genericPIV_2011, branded = total_2011_branded_index)
mean(total_2011_all$order)
var(total_2011_all$order)
fitdist(total_2011_all$order, "nbinom")

total_2010_all <- total_all_year(noPC = genericnoPIV_2010, PC = genericPIV_2010, branded = total_2010_branded_index)
mean(total_2010_all$order)
var(total_2010_all$order)
fitdist(total_2010_all$order, "nbinom")

total_2009_all <- total_all_year(noPC = genericnoPIV_2009, PC = genericPIV_2009, branded = total_2009_branded_index)
mean(total_2009_all$order)
var(total_2009_all$order)
fitdist(total_2009_all$order, "nbinom")

total_2008_all <- total_all_year(noPC = genericnoPIV_2008, PC = genericPIV_2008, branded = total_2008_branded_index)
mean(total_2008_all$order)
var(total_2008_all$order)
fitdist(total_2008_all$order, "nbinom")

total_2012_all <- total_all_year(noPC = genericnoPIV_2012, PC = genericPIV_2012, branded = total_2012_branded_index)
mean(total_2012_all$order)
var(total_2012_all$order)
fitdist(total_2012_all$order, "nbinom")

total_2013_all <- total_all_year(noPC = genericnoPIV_2013, PC = genericPIV_2013, branded = total_2013_branded_index)
mean(total_2013_all$order)
var(total_2013_all$order)
fitdist(total_2013_all$order, "nbinom")



all <- total_2011_all %>%
  rbind(total_2010_all) %>%
  rbind(total_2009_all) %>%
  rbind(total_2008_all)

mean(all$order)
var(all$order)
fitdist(all$order, "nbinom")



fit <- fitdist(total_before2012_all$order, "nbinom")

# get the fitted densities. mu and size from fit.
fitD <- dnbinom(0:16, size=2.234116, mu=4.203557)
pred_list = print(fitD)
predicted_df <- data.frame(n_pred = pred_list, order =total_before2012_count$order )

ggplot(total_before2012_count, aes(x=order, y=p)) + 
  geom_bar(stat="identity") +
  geom_line(color='red',data = predicted_df, aes(x=order, y=pred_list))

ggplot(total_2011_count, aes(x=order, y=p)) + 
  geom_bar(stat="identity") +
  geom_line(color='red',data = predicted_df, aes(x=order, y=pred_list))

ggplot(total_2010_count, aes(x=order, y=p)) + 
  geom_bar(stat="identity") +
  geom_line(color='red',data = predicted_df, aes(x=order, y=pred_list))

ggplot(total_2009_count, aes(x=order, y=p)) + 
  geom_bar(stat="identity") +
  geom_line(color='red',data = predicted_df, aes(x=order, y=pred_list))

ggplot(total_2008_count, aes(x=order, y=p)) + 
  geom_bar(stat="identity") +
  geom_line(color='red',data = predicted_df, aes(x=order, y=pred_list))

ggplot(total_2012_count, aes(x=order, y=p)) + 
  geom_bar(stat="identity") +
  geom_line(color='red',data = predicted_df, aes(x=order, y=pred_list))

ggplot(total_2013_count, aes(x=order, y=p)) + 
  geom_bar(stat="identity") +
  geom_line(color='red',data = predicted_df, aes(x=order, y=pred_list))

ggplot(total_2014_count, aes(x=order, y=p)) + 
  geom_bar(stat="identity") +
  geom_line(color='red',data = predicted_df, aes(x=order, y=pred_list))

ggplot(total_2015_count, aes(x=order, y=p)) + 
  geom_bar(stat="identity") +
  geom_line(color='red',data = predicted_df, aes(x=order, y=pred_list))

ggplot(total_2016_count, aes(x=order, y=p)) + 
  geom_bar(stat="identity") +
  geom_line(color='red',data = predicted_df, aes(x=order, y=pred_list))


# Goodness of fit with the chi squared test  
# get the frequency table
t <- table(genericPIV_before2012$order)   

# convert to dataframe
df <- as.data.frame(t)

# get frequencies
observed_freq <- df$Freq/300

# perform the chi-squared test
chisq.test(observed_freq, p=fitD)

library(MASS)

fitdistr(genericPIV_before2012$order, "Negative Binomial")

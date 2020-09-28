setwd("~/Dropbox/Advanced Method Project/Data")
source("Aim2/load_packages.R")

rm(list = ls())

#Load data from OB (products, Jan 2018)
#In txt file, need to exclude "'" in names to make sure all data could be imported
df<-read.table("products_2018.txt", sep = "~", header=TRUE, fill = TRUE)

#Change format of Application number and Product number
df$Appl_No<-formatC(df$Appl_No, width = 6, format = "d", flag = "0")
df$Product_No<-formatC(df$Product_No, width = 3, format = "d", flag = "0")

##Exclude discontinues or OTC drugs
df<-df[df$Type=="RX",]

#Clean data: Strength with extra notes
#Exclude those with "**Federal Register determination that product was not discontinued or withdrawn for safety or efficacy reasons**"
df$Strength<-as.character(df$Strength)
df$Strength1<- sapply(strsplit(df$Strength, split=' **', fixed=TRUE), function(x) (x[1]))
df$Strength2<- sapply(strsplit(df$Strength, split=' **', fixed=TRUE), function(x) (x[2]))

#Changed a few with slightly different wording than "Federal..."
df$Strength2<-ifelse(df$Strength2=="See current Annual Edition, 1.8 Description of Special Situations, Levothyroxine Sodium", NA, df$Strength2)
#Delete DNA 020903-001 "Indicated for use and comarketed with Interferon ALFA-2B, Recombinant (INTRON A), as Rebetron Combination Therapy**", as no info can be found in drugs@FDA
df$Strength2<-ifelse(df$Strength2=="Indicated for use and comarketed with Interferon ALFA-2B, Recombinant (INTRON A), as Rebetron Combination Therapy**", "Federal Register determination that product was not discontinued or withdrawn for safety or efficacy reasons**", df$Strength2)
df$Strength2<-ifelse(df$Strength2=="Federal Register notice that product was not discontinued or withdrawn for safety or efficacy reasons**", "Federal Register determination that product was not discontinued or withdrawn for safety or efficacy reasons**", df$Strength2)
df$Strength2<-ifelse(df$Strength2=="Federal Register determination that product was not withdrawn or discontinued for safety or efficacy reasons**", "Federal Register determination that product was not discontinued or withdrawn for safety or efficacy reasons**", df$Strength2)

df<-df[(is.na(df$Strength2)), ]
df<-df[, -c(15:16)]

#Change the format of EQ...BASE
df$Strength<- gsub("EQ ", "", df$Strength, fixed=TRUE)

df$Strength<- gsub(" BASE", "", df$Strength, fixed=TRUE)

#" BASE"
######Need to add more here to make strength consistent with NDC dataset


#Change format of dates of approval
df$date<-parse_date_time(df$Approval_Date, "bdY")
df$numdate<-as.integer(format(as.Date(df$date), "%Y%m%d"))

######Check missingness in Strength
#Drop if NA in date/ANDA/NDA Number: dropped 16 values
df<-df[!(is.na(df$Appl_No) | df$Appl_No==""), ]
df<-df[!(is.na(df$numdate) | df$numdate==""), ]

#Change format for RLD/not to dummy variable
df$RLD<-ifelse(df$RLD=="Yes", 1, 0)

#Sort by drug name, form, dosage, whether RLD, date of approval
df<-df[order(df[,1], df[,2], df[,5], df[,11], df[,16]),]

##Obtain formulation type
df$DF.Route<-as.character(df$DF.Route)
df$formulation<- sapply(strsplit(df$DF.Route, split=';', fixed=TRUE), function(x) (x[2]))
df$oral<-ifelse(df$formulation=="ORAL", 1, 0)
df$inject<-ifelse(df$formulation=="INJECTION", 1, 0)

##Add patent expiration info
##For the same APPL_NO and PRODUCT_NO, keep the first line (earliest date) 
##of PE_EXTENSION_DATE

patent <- read_excel("~/Dropbox/Advanced Method Project/Data/FDA/OB Historical List of Patents as of 08-20-18.xlsx")

table(patent$PE_EXTENSION_FLAG)
table(patent$DELIST_FLAG)
table(patent$DRUG_SUBSTANCE_FLAG)
#patent <- patent[patent[, "DELIST_FLAG"]=="N", ] (whehter to exclude delisted?)

patent <- patent[,c("APPL_NO", "PRODUCT_NO", "PE_EXTENSION_DATE", "EXPIRE_DATE")]

patent$EXPIRE_DATE <- parse_date_time(patent$EXPIRE_DATE, "%Y-%m-%d")
patent$EXPIRE_DATE <- strptime(as.character(patent$EXPIRE_DATE), "%Y-%m-%d")
patent$EXPIRE_DATE <- format(patent$EXPIRE_DATE, "%m/%d/%Y")
patent$PE_EXTENSION_DATE[is.na(patent$PE_EXTENSION_DATE)] <- as.character(patent$EXPIRE_DATE[is.na(patent$PE_EXTENSION_DATE)])

patent$PE_EXTENSION_DATE <- strptime(as.character(patent$PE_EXTENSION_DATE), "%m/%d/%Y")
patent$PE_EXTENSION_DATE <- format(patent$PE_EXTENSION_DATE, "%Y-%m-%d")

patent <- patent %>% group_by(APPL_NO, PRODUCT_NO) %>% summarise(patent=min(PE_EXTENSION_DATE, na.rm=T), latestpatent = max(PE_EXTENSION_DATE, na.rm=T)) %>% as.data.frame()

colnames(patent)[1] <- "Appl_No"
colnames(patent)[2] <- "Product_No"


df <- merge(df, patent, by=c("Appl_No","Product_No"), all.x=T)

#Test missingness of patent info
test <- df %>% filter(Appl_Type == "N")
test_na <- test %>% filter(is.na(patent))
nrow(test_na)/nrow(test)

###Add exclusivity dates for branded drugs
OBexclusivity <- read_excel("~/Dropbox/Advanced Method Project/Data/FDA/OB Exclusivity Expiration Date from 2000.xls")

#Keep Appl_No, Product No, Exclusivity type and Expiration dates
OBexclusivity <- OBexclusivity[, c("Appl No", "Product No", "Exclusivity Code", "Exclusivity Expire Date", "Appl Type")]
#Rename 
colnames(OBexclusivity)[1] <- "Appl_No"
colnames(OBexclusivity)[2] <- "Product_No"
colnames(OBexclusivity)[3] <- "Exclusivity_type"
colnames(OBexclusivity)[4] <- "Exclusivity_date"

#Include other exclusities for branded drug
#Compare exclusivity date with patent expiration dates
Brandexclusivity <- OBexclusivity[OBexclusivity[, "Appl Type"] == "N",]

#Change date into numbers, and rank to keep both the earliest and the latest ones
Brandexclusivity$numexclusivity <- as.integer(format(as.Date(Brandexclusivity$Exclusivity_date), "%Y%m%d"))

Brandexclusivity <- Brandexclusivity %>% group_by(Appl_No, Product_No) %>% summarise(earliestexclusivity=min(Exclusivity_date, na.rm=T), latestexclusivity = max(Exclusivity_date, na.rm=T)) %>% as.data.frame()

df <- merge(df, Brandexclusivity, by=c("Appl_No","Product_No"), all.x=T)



##Create a tibble for unique patents
#8254 unique drug ingredients, form and dosage (aka patent)
dfbyRLD <- df %>% group_by(Ingredient, DF.Route, Strength) %>% summarise(count=n()) %>% as.data.frame()
dfbyRLD$index <- seq.int(nrow(dfbyRLD))
as.data.frame(table(dfbyRLD$count))
#Merge: count means number of competitors; index: patent
df <- merge(df, dfbyRLD, by.x=c("Ingredient","DF.Route", "Strength"))

#Within each index, there might be more than one branded drug with the same ingredient(s),
#dosage, and route. (e.g. with flavor or PT) In this analysis, we only view
#the first approved branded drug as the reference one,
#and will exclud the second+ ones from the dataset to make sure their
#existence will not affect ordering of entry 

#First rank all the drugs
df <- df %>%
  group_by(index) %>%
  mutate(rank = rank(numdate,ties.method="min"))

branded_all <- df[df[, "Appl_Type"] == "N",]
generic_all <- df[df[, "Appl_Type"] == "A",]
branded_first <- branded_all %>% group_by(index) %>% filter(rank==1)
#only include first branded for each index
df <- rbind(branded_first, generic_all)
df <- subset(df, select=-c(rank))


#For each index, assign the same patent expiration date to all 
df <- df %>% group_by(index) %>% fill(patent) %>% fill(patent, .direction = "up")
df <- df %>% group_by(index) %>% fill(latestpatent) %>% fill(latestpatent, .direction = "up")

#For each index, assign the same branded drug exclusivity expiration date to all 
df <- df %>% group_by(index) %>% fill(earliestexclusivity) %>% fill(earliestexclusivity, .direction = "up")
df <- df %>% group_by(index) %>% fill(latestexclusivity) %>% fill(latestexclusivity, .direction = "up")

##Only keep index when branded drug has patent expiration info
#df <- df %>% group_by(index) %>% filter(!any(is.na(patent)))

##Among all patent/exclusivity expiration dates, get the earliest and latest dates
df <- df %>% mutate(min = pmin(as.Date(patent), as.Date(earliestexclusivity)))
df <- df %>% mutate(max = pmin(as.Date(latestpatent), as.Date(latestexclusivity)))

df$min[is.na(df$min) & is.na(df$earliestexclusivity)] <- as.Date(df$patent[is.na(df$min) & is.na(df$earliestexclusivity)])
df$min[is.na(df$min) & is.na(df$patent)] <- as.Date(df$earliestexclusivity[is.na(df$min) & is.na(df$patent)])

df$max[is.na(df$max) & is.na(df$latestexclusivity)] <- as.Date(df$latestpatent[is.na(df$max) & is.na(df$latestexclusivity)])
df$max[is.na(df$max) & is.na(df$latestpatent)] <- as.Date(df$latestexclusivity[is.na(df$max) & is.na(df$latestpatent)])

#incorporate AG infomation
library(readxl)
AG<- read_excel("FDA/AG/AG_20180328.xlsx")
AG$Number[is.na(AG$Number)] <- 0
AG<-AG[AG[, "Number"]!=0, ]

#Clean data: formulation
AG$Formulation[AG$Formulation=="EXTENDED-"] <- "ORAL"
AG$Formulation[AG$Formulation=="CAPSULES,"] <- "ORAL"
AG$Formulation[AG$Formulation=="DELAYED"] <- "ORAL"
AG$Formulation[AG$Formulation=="DELAYED-"] <- "ORAL"
AG$Formulation[AG$Formulation=="TABLET"] <- "ORAL"
AG$Formulation[AG$Formulation=="TABLETS"] <- "ORAL"
AG$Formulation[AG$Formulation=="FOR INJECTION"] <- "INJECTION"
AG$Formulation[AG$Formulation=="FOR ORAL"] <- "ORAL"
AG$Formulation[AG$Formulation=="ORAL SOLUTION"] <- "ORAL"
AG$Formulation[AG$Formulation=="ORALLY"] <- "ORAL"
AG$Formulation[AG$Formulation=="CAPSULE"] <- "ORAL"
AG$Formulation[AG$Formulation=="CAPSULES"] <- "ORAL"
AG$Formulation[AG$Formulation=="CHEWING GUM"] <- "ORAL"
AG$Formulation[AG$Formulation=="CHEWABLE"] <- "ORAL"
AG$Formulation[AG$Formulation=="TOPICAL GEL"] <- "TOPICAL"
AG$Formulation[AG$Formulation=="TABLET, FILM"] <- "ORAL"
AG$Formulation[AG$Formulation=="SUSTAINED-"] <- "ORAL"
AG$Formulation[AG$Formulation=="CREAM"] <- "TOPICAL"
AG$Formulation[AG$Formulation=="EMULSION,"] <- "TOPICAL"
AG$Formulation[AG$Formulation=="CREAM"] <- "TOPICAL"
AG$Formulation[AG$Formulation=="FILM COATED"] <- "TOPICAL"
AG$Formulation[AG$Formulation=="ER CAPSULES"] <- "TOPICAL"
AG$Formulation[AG$Formulation=="FILM,"] <- "TOPICAL"
AG$Formulation[AG$Formulation=="FLAVORED"] <- "ORAL"
AG$Formulation[AG$Formulation=="GELCAPS"] <- "ORAL"
AG$Formulation[AG$Formulation=="GRANULE"] <- "ORAL"
AG$Formulation[AG$Formulation=="GRANULES FOR"] <- "ORAL"
AG$Formulation[AG$Formulation=="IMMEDIATE-"] <- "ORAL"
AG$Formulation[AG$Formulation=="LOTION"] <- "TOPICAL"
AG$Formulation[AG$Formulation=="FOAM"] <- "TOPICAL"
AG$Formulation[AG$Formulation=="SHAMPOO"] <- "TOPICAL"
AG$Formulation[AG$Formulation=="FILM-COATED"] <- "ORAL"
AG$Formulation[AG$Formulation=="GEL"] <- "TOPICAL"
AG$Formulation[AG$Formulation=="LINGUAL"] <- "SUBLINGUAL"
AG$Formulation[AG$Formulation=="LINGUAL SPRAY"] <- "SUBLINGUAL"
AG$Formulation[AG$Formulation=="LOZENGE"] <- "TRANSMUCOSAL"
AG$Formulation[AG$Formulation=="NASAL SOLUTION"] <- "NASAL"
AG$Formulation[AG$Formulation=="NASAL SPRAY"] <- "NASAL"
AG$Formulation[AG$Formulation=="OINTMENT"] <- "TOPICAL"
AG$Formulation[AG$Formulation=="PATCH"] <- "TOPICAL"
AG$Formulation[AG$Formulation=="POWDER (FOR"] <- "ORAL"
AG$Formulation[AG$Formulation=="POWDER FOR"] <- "ORAL"
AG$Formulation[AG$Formulation=="POWDER, FOR"] <- "ORAL"
AG$Formulation[AG$Formulation=="RECTAL ENEMA"] <- "RECTAL"
AG$Formulation[AG$Formulation=="SPRAY"] <- "TOPICAL"
AG$Formulation[AG$Formulation=="SUSPENSION"] <- "ORAL"
AG$Formulation[AG$Formulation=="SUSPENSION/"] <- "ORAL"
AG$Formulation[AG$Formulation=="TOPICAL LOTION"] <- "TOPICAL"
AG$AG<-1

AG1<-AG[c(2,3,7)]
#delete duplicated rows
AG1<-unique(AG1[ , 1:3 ] )
colnames(AG1)[2] <- "formulation"
df<-merge(df, AG1, by=c("Trade_Name","formulation"), all.x=T)

df <- df %>% group_by(index) %>% fill(AG) %>% fill(AG, .direction = "up")
df$AG[is.na(df$AG)] <- 0

##Add REMS (ETASU) status
REMS <- read_excel("~/Dropbox/Advanced Method Project/Data/FDA/REMS_Versions.xlsx")
table(REMS$Elements_to_Assure_Safe_Use_Flag)
ETASU <- REMS %>% 
  filter(Elements_to_Assure_Safe_Use_Flag == 1) %>% 
  dplyr::select(REMS_Name, Version_Date)
colnames(ETASU)[1] <- "Trade_Name"
colnames(ETASU)[2] <- "ETASU_date"
ETASU$ETASU <- 1

#make drug names all capital
ETASU <- mutate_all(ETASU, funs(toupper))

df <- left_join(df, ETASU, by = "Trade_Name")
df <- df %>% group_by(index) %>% fill(ETASU) %>% fill(ETASU, .direction = "up")
df$ETASU[is.na(df$ETASU)] <- 0
df <- df %>% group_by(index) %>% fill(ETASU_date) %>% fill(ETASU_date, .direction = "up")

df$ETASU_lag <- as.Date(df$ETASU_date)-as.Date(df$date)
df$ETASU_after <- ifelse(df$ETASU_lag > 0, 1, 0)
df$ETASU_after[is.na(df$ETASU_after)] <- 0
table(df$ETASU_after, df$Appl_Type)
table(df$ETASU, df$Appl_Type)

##With guidance for ANDA application available
guidance <- read_excel("~/Dropbox/Advanced Method Project/Data/FDA/Product-Specific Guidances for Generic Drug Development.xlsx")

#For type (final or draft)
#guidance$Type <- as.character(guidance$Type)
guidance$Type <- gsub(" ", "", guidance$Type, fixed=TRUE)
guidance$Type <- str_replace_all(guidance$Type, fixed(" "), "")

#For application number
guidance$Appl_No <- gsub(" ", "", guidance$Appl_No, fixed=TRUE)
guidance$Appl_No <- str_pad(guidance$Appl_No, 6, pad = "0")

colnames(guidance)[which(names(guidance) == "Date")] <- "date_guidance"

guidance <- guidance %>% select(Appl_No, date_guidance)

#Add to main dataset
df <- left_join(df, guidance, by = "Appl_No")
df <- df %>% group_by(index) %>% fill(date_guidance) %>% fill(date_guidance, .direction = "up")

#Generate variables for guidance status
df$guidance_lag <- as.Date(df$date)-as.Date(df$date_guidance)
df$guidance_before <- ifelse(df$guidance_lag > 0, 1, 0)
df$guidance_before[is.na(df$guidance_before)] <- 0
table(df$guidance_before)


##Add information on generic manufacturer's size/power (April 2018)
size<- read_excel("Program Fee List (website)_04-30-2018.xlsx", skip = 1)
colnames(size)[1] <- "Applicant_Full_Name"
colnames(size)[2] <- "Tier"
colnames(size)[3] <- "ANDAnum"
ggplot(data = size, aes(x = ANDAnum)) +
  geom_histogram(aes(y =..density..), 
                 col="black",
                 fill="dodgerblue", 
                 bins = 100,
                 alpha=1) +
  xlab("Number of Approved ANDAs") +
  ylab("Percentage")

size <- size %>% select(1:2)
df <- left_join(df, size)
table(df$Tier)

df$Tier[is.na(df$Tier)] <- "missing/NA"
table(df$Tier)

#df$Tier <- relevel(df$Tier, ref = "Small")




##Add NDC codes
setwd("~/Dropbox/Advanced Method Project/Data/ndcxls_20180706")
NDC <- read_excel("product.xlsx")
NDC_package <- read_excel("package.xlsx")
NDC_package <- NDC_package[, c(1, 3)]
#NDC <- merge(NDC, NDC_package, by=c("PRODUCTID"), all.x=T)
#NDC <- NDC[!duplicated(NDC),]

#View(NDC)

setwd("~/Dropbox/Advanced Method Project/Data")
#ATC therapeutic class info
NDC_ATC <- read_excel("NDC ATC mapping.xlsx")
NDC_ATC$PRODUCTNDC <- str_sub(NDC_ATC$NDC, 1, 9) #Keep only first 9 digits

NDC_ATC <- NDC_ATC %>% group_by(PRODUCTNDC) %>% distinct(ATC4)
NDC_ATC <- NDC_ATC %>% mutate(ATC1 = str_sub(ATC4, 1, 1))
NDC_ATC <- NDC_ATC %>% mutate(ATCA = ifelse(ATC1 == "A", 1, 0))
NDC_ATC <- NDC_ATC %>% mutate(ATCB = ifelse(ATC1 == "B", 1, 0))
NDC_ATC <- NDC_ATC %>% mutate(ATCC = ifelse(ATC1 == "C", 1, 0))
NDC_ATC <- NDC_ATC %>% mutate(ATCD = ifelse(ATC1 == "D", 1, 0))
NDC_ATC <- NDC_ATC %>% mutate(ATCG = ifelse(ATC1 == "G", 1, 0))
NDC_ATC <- NDC_ATC %>% mutate(ATCH = ifelse(ATC1 == "H", 1, 0))
NDC_ATC <- NDC_ATC %>% mutate(ATCJ = ifelse(ATC1 == "J", 1, 0))
NDC_ATC <- NDC_ATC %>% mutate(ATCL = ifelse(ATC1 == "L", 1, 0))
NDC_ATC <- NDC_ATC %>% mutate(ATCM = ifelse(ATC1 == "M", 1, 0))
NDC_ATC <- NDC_ATC %>% mutate(ATCN = ifelse(ATC1 == "N", 1, 0))
NDC_ATC <- NDC_ATC %>% mutate(ATCP = ifelse(ATC1 == "P", 1, 0))
NDC_ATC <- NDC_ATC %>% mutate(ATCR = ifelse(ATC1 == "R", 1, 0))
NDC_ATC <- NDC_ATC %>% mutate(ATCS = ifelse(ATC1 == "S", 1, 0))
NDC_ATC <- NDC_ATC %>% mutate(ATCV = ifelse(ATC1 == "V", 1, 0))


#NDC_ATC <- NDC_ATC %>% group_by(PRODUCTNDC) %>% summarise(ATC = paste(ATC4, collapse=", "))
df$formulation<- sapply(strsplit(df$DF.Route, split=';', fixed=TRUE), function(x) (x[2]))


#Map this info back to NDC dataset
NDC <- merge(NDC, NDC_ATC, by=c("PRODUCTNDC"), all.x=T)
table(NDC$ATC1)

#Keep NDA, ANDA and Authorized Generics
#Did not consider OTC MONOGRAPH FINAL
NDC<-NDC[( NDC$MARKETINGCATEGORYNAME=="NDA" | NDC$MARKETINGCATEGORYNAME=="ANDA" | NDC$MARKETINGCATEGORYNAME=="NDA AUTHORIZED GENERIC"), ]

#Exclude those that have been excluded
NDC<-NDC[ NDC$NDC_EXCLUDE_FLAG=="N", ]

#Exclude KIT (no information on Strength)
NDC<-NDC[ !(NDC$DOSAGEFORMNAME=="KIT"), ]

#Get Appl_No
NDC$Appl_No<-str_sub(NDC$APPLICATIONNUMBER, -6, -1)

#Exclude biologics (Application number starting with BN)
NDC$Appl_type<-str_sub(NDC$APPLICATIONNUMBER, -9, -7)
NDC<-NDC[(! NDC$Appl_type=="BA" | ! NDC$Appl_type=="BN"), ]
NDC <- NDC[, -38]

#Keep prescription drug only
table(NDC$PRODUCTTYPENAME)
NDC<-NDC[NDC$PRODUCTTYPENAME=="HUMAN PRESCRIPTION DRUG", ]

##Method 3: directly map by NDA/ANDA numbers 
class <- NDC[, c(17, 21:37)]
class <- class[!duplicated(class),]
NDC_to_ApplNo <- NDC[, c(1, 23)]
#Export this dataset
write.csv(NDC_to_ApplNo, 'NDC_to_ApplNo.csv')

#Try to keep only 1 class per Appl_No
class <- class[!duplicated(class$Appl_No),]


df <- merge(df, class, by=c("Appl_No"), all.x=T)
df <- df[!duplicated(df),]

colnames(df)[which(names(df) == "PHARM_CLASSES")] <- "class"

#Within the same index, assign ATC1 code to those with missingness
df <- df %>% group_by(index) %>% fill(ATCA) %>% fill(ATCA, .direction = "up")
df <- df %>% group_by(index) %>% fill(ATCA) %>% fill(ATCA, .direction = "down")
df <- df %>% group_by(index) %>% fill(ATCB) %>% fill(ATCB, .direction = "up")
df <- df %>% group_by(index) %>% fill(ATCB) %>% fill(ATCB, .direction = "down")
df <- df %>% group_by(index) %>% fill(ATCC) %>% fill(ATCC, .direction = "up")
df <- df %>% group_by(index) %>% fill(ATCC) %>% fill(ATCC, .direction = "down")
df <- df %>% group_by(index) %>% fill(ATCD) %>% fill(ATCD, .direction = "up")
df <- df %>% group_by(index) %>% fill(ATCD) %>% fill(ATCD, .direction = "down")
df <- df %>% group_by(index) %>% fill(ATCG) %>% fill(ATCG, .direction = "up")
df <- df %>% group_by(index) %>% fill(ATCG) %>% fill(ATCG, .direction = "down")
df <- df %>% group_by(index) %>% fill(ATCH) %>% fill(ATCH, .direction = "up")
df <- df %>% group_by(index) %>% fill(ATCH) %>% fill(ATCH, .direction = "down")
df <- df %>% group_by(index) %>% fill(ATCJ) %>% fill(ATCJ, .direction = "up")
df <- df %>% group_by(index) %>% fill(ATCJ) %>% fill(ATCJ, .direction = "down")
df <- df %>% group_by(index) %>% fill(ATCL) %>% fill(ATCL, .direction = "up")
df <- df %>% group_by(index) %>% fill(ATCL) %>% fill(ATCL, .direction = "down")
df <- df %>% group_by(index) %>% fill(ATCM) %>% fill(ATCM, .direction = "up")
df <- df %>% group_by(index) %>% fill(ATCM) %>% fill(ATCM, .direction = "down")
df <- df %>% group_by(index) %>% fill(ATCN) %>% fill(ATCN, .direction = "up")
df <- df %>% group_by(index) %>% fill(ATCN) %>% fill(ATCN, .direction = "down")
df <- df %>% group_by(index) %>% fill(ATCP) %>% fill(ATCP, .direction = "up")
df <- df %>% group_by(index) %>% fill(ATCP) %>% fill(ATCP, .direction = "down")
df <- df %>% group_by(index) %>% fill(ATCR) %>% fill(ATCR, .direction = "up")
df <- df %>% group_by(index) %>% fill(ATCR) %>% fill(ATCR, .direction = "down")
df <- df %>% group_by(index) %>% fill(ATCS) %>% fill(ATCS, .direction = "up")
df <- df %>% group_by(index) %>% fill(ATCS) %>% fill(ATCS, .direction = "down")
df <- df %>% group_by(index) %>% fill(ATCV) %>% fill(ATCV, .direction = "up")
df <- df %>% group_by(index) %>% fill(ATCV) %>% fill(ATCV, .direction = "down")




df$route <- df$formulation
df$route[df$formulation == "ORAL-28"] <- "ORAL"
df$route[! (df$formulation == "ORAL" | df$formulation == "INJECTION")] <- "OTHER"
#change reference to other 
df$route <- as.factor(df$route)
df$route <- relevel(df$route, ref = "ORAL")



#Export this dataset
write.csv(df, 'OB.csv')



############################################################################
######################Merge data (PC Exclusibity)######################################
#From 2000 onwards
############################################################################
#PC only
PCexclusivity <- OBexclusivity[OBexclusivity[, "Exclusivity_type"] == "PC",]
PCexclusivity <- PCexclusivity[, -5]
colnames(PCexclusivity)[3] <- "PCExclusivity_type"
colnames(PCexclusivity)[4] <- "PCExclusivity_date"


generic <- df %>% filter(Appl_Type == "A")
#FF: first filers that enjoyed PC exclusivity 
#557 observations, but only 484 can be matched with the current OB info for ANDA # and Product #
FF <- inner_join(generic, PCexclusivity, by=c("Appl_No","Product_No"))
#Change format of dates of end of exclusivity
colnames(FF)[which(names(FF) == "PCExclusivity_date")] <- "exclusivity"


##time: difference between approval and end of exclusivity
FF$time<-as.Date(FF$exclusivity)-as.Date(FF$date)
FF<-FF[FF$time>=0,] #exclude 2 cases
hist(as.numeric(FF$time))
FF1<-FF[FF$time<=180,]
#FF<-FF[FF$time<=200,]
##Q: why so many with time >180, or with negative time?
##Shall we assume 0<time<200?
#FF<-FF[FF$time<=200,]
#FF<-FF[FF$time>=0,]
##Generate indicator for having PIV entry
FF$PIV<-c(rep(1, nrow(FF)))
##Merge back to the main data (generic)
generic<-merge(generic, FF, all=TRUE)

#Approval year
generic$approveyear<-floor(generic$numdate/10000)
#Approval date
generic$approvemonth<-floor(generic$numdate/100)

#Add order of entry
generic <- generic %>%
  group_by(index) %>%
  mutate(order = rank(numdate,ties.method="first"))

##Only look at between 2007-10-01 and 2017-09-31
#in specific, first generic need to be after 2007-10-01
#while all need to be before 2017-10-01
#also need to restrict to index date on or after 2007-10-01; will update in PC and noPC files
generic1_index <- generic %>% group_by(index) %>% filter(order == 1 & numdate > 20071000) %>% select(index)
generic <- inner_join(generic, generic1_index)

generic <- generic %>% 
  filter(numdate < 20171001)

save(df, file = "all.RData")
save(generic, file = "generic.RData")


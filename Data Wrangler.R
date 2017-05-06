#Session Session
setwd("~/Documents/Personal Files/Project/Data")

#Librabries
library(dplyr)
library(ggplot2)
library(readxl)
library(hashmap)

#Read Data to raw data frame
df16_raw <- read.csv("Data_FY16.csv", head=TRUE) # 6,47,852 rows
df15_raw <- read.csv("Data_FY15.csv", head=TRUE) # 6,18,804
df14_raw <- read.csv("Data_FY14.csv", head=TRUE) # 5,19,504
df13_raw <- read.csv("Data_FY13.csv", head=TRUE) # 4,42,275
df12_raw <- read.csv("Data_FY12.csv", head=TRUE) # 4,15,845
df11_raw <- read.csv("Data_FY11.csv", head=TRUE) # 3,58,857


#Change Column Names
temp14 <- df11_raw

colnames(temp14)[1] <- "CASE_NUMBER"
colnames(temp14)[2] <- "CASE_STATUS"
colnames(temp14)[3] <- "CASE_SUBMITTED"
colnames(temp14)[8] <- "EMPLOYER_NAME"
colnames(temp14)[10] <- "EMPLOYER_CITY"
colnames(temp14)[11] <- "EMPLOYER_STATE"
colnames(temp14)[13] <- "SOC_CODE"
colnames(temp14)[15] <- "JOB_TITLE"
colnames(temp14)[16] <- "WAGE_RATE_OF_PAY_FROM"
colnames(temp14)[17] <- "WAGE_RATE_OF_PAY_TO"
colnames(temp14)[18] <- "WAGE_UNIT_OF_PAY"
colnames(temp14)[19] <- "FULL_TIME_POSITION"
colnames(temp14)[21] <- "WORKSITE_CITY"
colnames(temp14)[22] <- "WORKSITE_STATE"
colnames(temp14)[23] <- "PREVAILING_WAGE"
colnames(temp14)[24] <- "PW_UNIT_OF_PAY"
colnames(temp14)[35] <- "NAIC_CODE"

df11 <- temp14

colnames(df15_raw)[33] <- "WAGE_RATE_OF_PAY_FROM"

#Select Required Columns

#2016
df16 <- df16_raw[,c("CASE_STATUS","EMPLOYER_NAME",
                    "EMPLOYER_CITY","EMPLOYER_STATE","JOB_TITLE", "SOC_CODE", "FULL_TIME_POSITION",
                    "PREVAILING_WAGE","PW_UNIT_OF_PAY","WAGE_RATE_OF_PAY_FROM",
                    "WAGE_UNIT_OF_PAY","WORKSITE_CITY","WORKSITE_STATE")]
#2015
df15 <- df15_raw[,c("CASE_STATUS","EMPLOYER_NAME",
                    "EMPLOYER_CITY","EMPLOYER_STATE","JOB_TITLE", "SOC_CODE", "FULL_TIME_POSITION",
                    "PREVAILING_WAGE","PW_UNIT_OF_PAY","WAGE_RATE_OF_PAY_FROM",
                    "WAGE_UNIT_OF_PAY","WORKSITE_CITY","WORKSITE_STATE")]

#2014
df14 <- df14[,c("CASE_STATUS","EMPLOYER_NAME",
                    "EMPLOYER_CITY","EMPLOYER_STATE","JOB_TITLE", "SOC_CODE", "FULL_TIME_POSITION",
                    "PREVAILING_WAGE","PW_UNIT_OF_PAY","WAGE_RATE_OF_PAY_FROM",
                    "WAGE_UNIT_OF_PAY","WORKSITE_CITY","WORKSITE_STATE")]
#2013
df13 <- df13[,c("CASE_STATUS","EMPLOYER_NAME",
                "EMPLOYER_CITY","EMPLOYER_STATE","JOB_TITLE", "SOC_CODE", "FULL_TIME_POSITION",
                "PREVAILING_WAGE","PW_UNIT_OF_PAY","WAGE_RATE_OF_PAY_FROM",
                "WAGE_UNIT_OF_PAY","WORKSITE_CITY","WORKSITE_STATE")]
#2012
df12 <- df12[,c("CASE_STATUS","EMPLOYER_NAME",
                "EMPLOYER_CITY","EMPLOYER_STATE","JOB_TITLE", "SOC_CODE", "FULL_TIME_POSITION",
                "PREVAILING_WAGE","PW_UNIT_OF_PAY","WAGE_RATE_OF_PAY_FROM",
                "WAGE_UNIT_OF_PAY","WORKSITE_CITY","WORKSITE_STATE")]
#2013
df11 <- df11[,c("CASE_STATUS","EMPLOYER_NAME",
                "EMPLOYER_CITY","EMPLOYER_STATE","JOB_TITLE", "SOC_CODE", "FULL_TIME_POSITION",
                "PREVAILING_WAGE","PW_UNIT_OF_PAY","WAGE_RATE_OF_PAY_FROM",
                "WAGE_UNIT_OF_PAY","WORKSITE_CITY","WORKSITE_STATE")]

#Add Year Column to Each data
df16 <- data.frame(YEAR = rep("2016", nrow(df16)), df16[,])


#Convert Factor df16$PREVAILING_WAGE to numberic
temp <- df15
fac <- as.numeric( sub(",", ".", fac) )
df16$PREVAILING_WAGE <- as.numeric(sub(",","", df16$PREVAILING_WAGE))
df16$WAGE_RATE_OF_PAY_FROM <- as.numeric(sub(",","", df16$WAGE_RATE_OF_PAY_FROM))

df15$WAGE_RATE_OF_PAY_FROM <- as.numeric(sub(" - *","", df15$WAGE_RATE_OF_PAY_FROM))

df15 <- temp


#Combine Data Frames

finaldata = data.frame()

finaldata = rbind(finaldata, df16)
finaldata = rbind(finaldata, df15)
finaldata = rbind(finaldata, df14)
finaldata = rbind(finaldata, df13)
finaldata = rbind(finaldata, df12)
finaldata = rbind(finaldata, df11)

saveRDS(finaldata,"finaldata.rds")

write.csv(file="finaldata.csv", x=finaldata)


#################################
data <- read.csv("finaldata.csv")
library(dplyr)
library(ggplot2)
library(readxl)
library(hashmap)
library(stringr)

#Only CERTIFIED and DENIED
newdata <- subset(data, (CASE_STATUS == "CERTIFIED"  
                         |CASE_STATUS == "DENIED"))


#Finding out % of Unit of pay in Prewailing wage rate
newdata %>%
  group_by(PW_UNIT_OF_PAY) %>%
  summarise(count = n(), percentage = 100*count/(dim(newdata)[1]))


#Function to calculate Annual prevailing wage
pw_unit_to_yearly <- function(prevailing_wage, pw_unit_of_pay) {
  return(ifelse(pw_unit_of_pay == "Year", 
                prevailing_wage, 
                ifelse(pw_unit_of_pay == "Hour", 
                       2080*prevailing_wage, 
                       ifelse(pw_unit_of_pay== "Week", 
                              52*prevailing_wage, 
                              ifelse(pw_unit_of_pay == "Month", 
                                     12*prevailing_wage, 
                                     26*prevailing_wage)))))
}


#Iterating through data to find different PW unit of pay
newdata %>%
  filter(!is.na(PW_UNIT_OF_PAY)) %>%
  mutate(PREVAILING_WAGE = as.numeric(as.character(newdata$PREVAILING_WAGE))) %>%
  mutate(PREVAILING_WAGE =  pw_unit_to_yearly(PREVAILING_WAGE, PW_UNIT_OF_PAY)) %>%
  select(- PW_UNIT_OF_PAY) -> newdata


#Function to calculate Annual wage from 
wage_unit_to_yearly <- function(WAGE_RATE_OF_PAY_FROM, WAGE_UNIT_OF_PAY) {
  return(ifelse(WAGE_UNIT_OF_PAY == "Year", 
                WAGE_RATE_OF_PAY_FROM, 
                ifelse(WAGE_UNIT_OF_PAY == "Hour", 
                       2080*WAGE_RATE_OF_PAY_FROM, 
                       ifelse(WAGE_UNIT_OF_PAY== "Week", 
                              52*WAGE_RATE_OF_PAY_FROM, 
                              ifelse(WAGE_UNIT_OF_PAY == "Month", 
                                     12*WAGE_RATE_OF_PAY_FROM, 
                                     26*WAGE_RATE_OF_PAY_FROM)))))
}




#Iterating through data to find different wage unit of pay
newdata %>%
  filter(!is.na(WAGE_RATE_OF_PAY_FROM) && !is.na(WAGE_UNIT_OF_PAY)) %>%
  mutate(WAGE_RATE_OF_PAY_FROM = as.numeric(as.character(newdata$WAGE_RATE_OF_PAY_FROM))) %>%
  mutate(WAGE_RATE_OF_PAY_FROM =  wage_unit_to_yearly(newdata$WAGE_RATE_OF_PAY_FROM, newdata$WAGE_UNIT_OF_PAY)) %>%
  select(- WAGE_UNIT_OF_PAY) -> newdata


newdata %>%
  group_by(FULL_TIME_POSITION) %>%
  summarise(count = n(),percentage = 100*count/(dim(newdata)[1]))


# Generic ggplot graphics configuration I will be using for all my plots
get_theme <- function() {
  return(theme(axis.title = element_text(size = rel(1.5)),
               legend.position = "bottom",
               legend.text = element_text(size = rel(1.5)),
               legend.title = element_text(size=rel(1.5)),
               axis.text = element_text(size=rel(1.5)))) 
}

# Avoid scientific notation in plot
options(scipen = 999)

g <- ggplot(data = newdata, aes(x=YEAR, y = PREVAILING_WAGE))
g <- g + geom_boxplot(aes(fill=FULL_TIME_POSITION)) + coord_cartesian(ylim=c(0,125000))
g <- g + xlab("YEAR") + ylab("WAGE (USD)") + get_theme()

newdata %>%
  group_by(FULL_TIME_POSITION) %>%
  summarise('75%' = quantile(PREVAILING_WAGE,probs = 0.75,na.rm=TRUE))



newdata$FULL_TIME_POSITION <- as.character(newdata$FULL_TIME_POSITION)



newdata %>% 
  mutate(FULL_TIME_POSITION = ifelse(is.na(FULL_TIME_POSITION), 
                                     ifelse(PREVAILING_WAGE > 70000,'Y','N'), 
                                     FULL_TIME_POSITION)) -> newdata

newdata$FULL_TIME_POSITION <- as.factor(newdata$FULL_TIME_POSITION)

#read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
state_abbs = c("AK", "AL", "AR","AS", "AZ", "CA", "CO", "CT", "DC", "DE", "FL","FM", "GA","GU",
               "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME","MH",
               "MI", "MN", "MO","MP", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
               "NV", "NY", "OH", "OK", "OR", "PA", "PR","PW", "RI", "SC", "SD", "TN",
               "TX", "UT", "VA","VI", "VT", "WA", "WI", "WV", "WY")

state_full = c("alaska","alabama","arkansas","american samoa","arizona","california","colorado",
               "connecticut","district of columbia","delaware","florida","Federated States of Micronesia","georgia","Guam",
               "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
               "louisiana","massachusetts","maryland","maine","Marshall Islands","michigan","minnesota",
               "missouri","Northern Mariana Islands","mississippi","montana","north carolina","north dakota",
               "nebraska","new hampshire","new jersey","new mexico","nevada",
               "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico","Palau",
               "rhode island","south carolina","south dakota","tennessee","texas",
               "utah","virginia","Virgin Islands","vermont","washington","wisconsin",
               "west virginia","wyoming")

state_hash = hashmap(state_abbs,state_full)


newdata$WORKSITE_STATE_FULL = sapply(newdata$WORKSITE_STATE, function(x,y) {return(toupper(y[[x]]))}, y = state_hash)


newdata %>% 
  group_by(WORKSITE_CITY) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) -> sites_count



install.packages("ggmap")
library(ggmap)

top_sites <- (sites_count$WORKSITE_CITY)[1:2500]

top_sites <- as.character(top_sites)

site_geocodes <- cbind(geocode(top_sites),top_sites)

#newdata$WORKSITE_CITY[, !sapply(newdata$WORKSITE_CITY, is.numeric)]


site_geocodes %>%
  rename(WORKSITE_CITY = top_sites) -> site_geocodes

saveRDS(site_geocodes,"geocodes.RDS")

site_geocodes <- readRDS("geocodes.RDS")

N_sites_geocoded <- dim(site_geocodes)[1]

share <- 100*sum((sites_count$count)[1:N_sites_geocoded])/(dim(newdata)[1])

print(paste0("Records captured by geocoded sites: ", share))

newdata <- full_join(newdata,site_geocodes,by="WORKSITE_CITY")

row.has.na <- apply(newdata, 1, function(newdata){any(is.na(newdata))})
d <- sum(row.has.na)

datana <- newdata[!row.has.na,]

saveRDS(newdata,"h1b_transformed.rds")
saveRDS(datana,"h1b_transformed_without_na.rds")

h1b_transformed <- readRDS("h1b_transformed.rds")

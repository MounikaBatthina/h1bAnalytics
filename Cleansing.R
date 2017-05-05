data <- read.csv(paste("C:/Users/maruthi/Dropbox/Masters/Spring17/239/Project/",'finaldata.csv',sep=""),header = TRUE)
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

g 

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


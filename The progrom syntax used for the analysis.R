library(dplyr)
library(tidyverse)
library(magrittr)
library(plm)
# “mydata.csv” is the processed data

mydata <- na.omit(read.csv('mydata.csv'))#read data
str(mydata)#check data frame
mydata$MONTH <- (sprintf("%s",mydata$MONTH)) #transfer data format
mydata1 <- mydata %>% unite('yearmonth', YEAR:MONTH, sep = '.')
mydata1$YM <- as.numeric(mydata1$yearmonth)
mydata1$AirlineID <- 1:nrow(mydata1)#Airline ID
str(mydata1)
mydata1.cha <- mydata1[c(2:7)]
mydata1.num <- mydata1[c(1,7:15)]
for(i in 2:6){
  tdat <- mydata1[c(i,7)] 
 p <- ggplot(tdat, aes(x = tdat[[1]], y = tdat[[2]]))+
   geom_point()+
   ylab(colnames(mydata1)[i])
   print(p)
}

rankData1<-pdata.frame(mydata1,index=c("AirlineID","yearmonth"))

#Model1
Model1 <- plm(ARRIVAL_DELAY ~ DISTANCE+ FREQUENCY+CARRIER_DELAY+ WEATHER_DELAY+ NAS_DELAY+ SECURITY_DELAY
              + LATE_AIRCRAFT_DELAY
              ,data=rankData1,effect = "time",model="within")
summary(Model1)
# sensitive analysis for model 1
Sensitive_data1 <- mydata1 %>% mutate(Ln_Frequency = log(FREQUENCY))
Sensitive_data11 <- Sensitive_data1 %>% mutate(Ln_arrivaldelay = log(ARRIVAL_DELAY),Ln_carrierdelay=log(CARRIER_DELAY),Ln_weatherdelay=log(WEATHER_DELAY),Ln_NASdelay=log(NAS_DELAY),Ln_securitydelay=log(SECURITY_DELAY),Ln_lateaircraftdelay=log(LATE_AIRCRAFT_DELAY))
str(Sensitive_data11)


Sensitive_data111 <- lapply(Sensitive_data11, function(x) replace(x, is.infinite(x), 0))
str(Sensitive_data111)
rankData1_sensitive11<-pdata.frame(Sensitive_data111,index=c("AirlineID","yearmonth"))


Model11 <- plm(Ln_arrivaldelay ~ DISTANCE+ Ln_Frequency+Ln_carrierdelay+ Ln_weatherdelay+Ln_NASdelay+Ln_securitydelay
              + Ln_lateaircraftdelay
              ,data=rankData1_sensitive11,effect = "time",model="within")
summary(Model11)
 


#----------------------------------------------------------------------------------------------------

#Model2
#Whether the merge occurs , filter according to the closing date of the 5 merge cases
mydata2 <- mydata1 %>% mutate(
  merged_dummy = case_when(
    yearmonth < 2010.01 & OP_UNIQUE_CARRIER == 'DL' ~ 0,
    yearmonth < 2011.01 & OP_UNIQUE_CARRIER == 'UA' ~ 0,
    yearmonth < 2013.09 & OP_UNIQUE_CARRIER == 'AA' ~ 0,
    yearmonth < 2015.01 & OP_UNIQUE_CARRIER == 'WN' ~ 0,
    yearmonth < 2017.01 & OP_UNIQUE_CARRIER == 'AS' ~ 0,
    T ~ 1
  )
)


rankData2 <-pdata.frame(mydata2,index=c("AirlineID","yearmonth"))
Model2 <- plm(ARRIVAL_DELAY ~  DISTANCE+ FREQUENCY +merged_dummy
               ,data=rankData2,effect = "time",model="within")
summary(Model2)




mydata2 %>% group_by(merged,OP_UNIQUE_CARRIER, .add = T) %>% 
  summarise(means = mean(ARRIVAL_DELAY ))%>%
  ggplot(aes(x = factor(merged) , y = means, fill = factor(merged)))+
  geom_histogram(stat = 'identity')+
  facet_grid(~OP_UNIQUE_CARRIER,scales = 'free')



               
# Model 2  Robustness Analysis



# method1 sensitive analysis for model2
sensitivity_results <- list()


sensitive_variables <- c("DISTANCE","FREQUENCY")

for (variable in sensitive_variables) {
  # change variable value
  rankData2_sensitive <- rankData2
  rankData2_sensitive[, variable] <- rankData2_sensitive[, variable] * 1.1}
  
# refitting model
Model2_sensitive <- plm(ARRIVAL_DELAY ~ DISTANCE +FREQUENCY+ merged, data = rankData2_sensitive, effect = "time", model = "within")
  
# Store model results in the sensitivity analysis results list
  sensitivity_results[[variable]] <- summary(Model2_sensitive)
  
summary(Model2_sensitive)
# method2 sensitive analysis for model2

Sensitive_data2 <- mydata2 %>% mutate(Ln_arrivaldelay = log(ARRIVAL_DELAY),Ln_Frequency = log(FREQUENCY))
str(Sensitive_data2)
rankData2_sensitive2<-pdata.frame(Sensitive_data2,index=c("AirlineID","yearmonth"))
Model2_sensitive2<-plm(Ln_arrivaldelay ~ DISTANCE +Ln_Frequency+ merged_dummy, data = rankData2_sensitive2, effect = "time", model = "within")
summary(Model2_sensitive2)
#------------------------------------------------------------------------------------------------------


# Model 3
#The closing date of the 5 merger cases is used as the date of the merger
#and the closing date starts 2 years later, that is, 0-2 years is short-term, and 3-5 years is long-term

mydata3 <- mydata2 %>% mutate(
  Merged02_DL = case_when(yearmonth <= 2012.01 & OP_UNIQUE_CARRIER == 'DL' ~ 2),
  Merged02_UA =case_when(yearmonth <= 2013.1 & OP_UNIQUE_CARRIER == 'UA' ~ 2),
  Merged02_AA =case_when (yearmonth <= 2015.09 & OP_UNIQUE_CARRIER == 'AA' ~ 2),
  Merged02_WN  =case_when(yearmonth <= 2017.01 & OP_UNIQUE_CARRIER == 'WN' ~ 2),
  Merged02_AS  =case_when(yearmonth <= 2019.01 & OP_UNIQUE_CARRIER == 'AS' ~ 2),
  Merged35_DL =case_when(yearmonth < 2015.01 & 2012.01< yearmonth &OP_UNIQUE_CARRIER == 'DL' ~ 5),
  Merged35_UA =case_when(yearmonth < 2016.01 & 2013.1< yearmonth &OP_UNIQUE_CARRIER == 'UA' ~ 5),
  Merged35_AA =case_when(yearmonth < 2018.01 & 2015.09< yearmonth &OP_UNIQUE_CARRIER == 'AA' ~ 5),
  Merged35_WN =case_when(yearmonth < 2020.01 & 2017.01 < yearmonth &OP_UNIQUE_CARRIER == 'WN' ~ 5),
  Merged35_AS =case_when(yearmonth < 2023.01 & 2019.01< yearmonth &OP_UNIQUE_CARRIER == 'AS' ~ 5),T=0)
str(mydata3)

# Replace the NA value with 0
clean_mydata3 <- replace(mydata3, is.na(mydata3), 0)


# View the replaced dataset
str(clean_mydata3)

mydata3 <- clean_mydata3[, colSums(clean_mydata3 != 0) > 0]
str(mydata3)
rankData3 <-pdata.frame(mydata3,index=c("AirlineID","yearmonth"))
Model3 <- plm(ARRIVAL_DELAY ~  DISTANCE+ FREQUENCY+ Merged02_DL+Merged02_UA+Merged02_AA+Merged02_WN+Merged02_AS+Merged35_DL+Merged35_UA+Merged35_AA+Merged35_WN+Merged35_AS
              ,data=rankData3,effect = "time",model="within")
summary(Model3)

#sensitive model 3

Sensitive_model3_data <- mydata3 %>% mutate(Ln_Frequency = log(FREQUENCY),Ln_arrivaldelay = log(ARRIVAL_DELAY))

str(Sensitive_model3_data)
sensitiverankData3 <-pdata.frame(Sensitive_model3_data,index=c("AirlineID","yearmonth"))

sensitive_Model3 <- plm(Ln_arrivaldelay ~  DISTANCE+ Ln_Frequency+ Merged02_DL+Merged02_UA+Merged02_AA+Merged02_WN+Merged02_AS+Merged35_DL+Merged35_UA+Merged35_AA+Merged35_WN+Merged35_AS
              ,data=sensitiverankData3,effect = "time",model="within")
summary(sensitive_Model3)





mydata3 %>% group_by(Merged02_DL,Merged02_UA,Merged02_AA,Merged02_WN,
                     Merged02_AS,Merged35_DL, Merged35_UA,Merged35_AA,Merged35_WN,Merged35_AS, OP_UNIQUE_CARRIER, .add = T) %>% 
  summarise(means = mean(ARRIVAL_DELAY ))

#finish-------------------------------------------------------------------------



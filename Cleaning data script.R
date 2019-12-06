#load the R Libraries cores
#Check if libraries are installed, if not then install and load
if (!require(haven)){
  install.packages("haven")
  library(haven)
  }

#Check if tidyverse is installed
if(!require(tidyverse)){
  #Install tidyverse if it isn't available and load it after
  install.packages("tidyverse")
  library(tidyverse)
  }

#Begin to initiate data into tidyverse
Ethnic_Data <- read.csv("EthnicData.csv")
female_income<-read.csv("FemaleIncome.csv")
female_Regional<-read.csv("FemaleRegionalData.csv")
Total_Gender_Income<-read.csv("GenderIncomesTotal.csv")
male_income<-read.csv("MaleIncome.csv")
male_regional<-read.csv("MaleRegionalData.csv")
regional_income<-read.csv("RegionalIncome.csv")

#Clean unnecessary data from Ethnic Data
Ethnic_Data %>%
  select(-Median.Weekly.Earnings, -Number.of.People, -Median.Hourly.Earnings) -> EthnicDataCleaned

#Begin to filter out unncessary data in the female income dataset
female_income %>%
  select(-Number.of.People..000., -Median.Weekly.Earnings,- Median.Hourly.Earnings) -> CleanFemaleIncome
CleanFemaleIncome <- gather(CleanFemaleIncome, key ="Average Earnings", value = "Rate of Income", -Year)

#Filter out unnecessary columns in the female regional data
female_Regional %>%
  select(-Median.hourly.earnings, -Number.of.people, -Medain.Weekly.earnings) -> CleanFemalebyRegion
CleanFemalebyRegion <- gather(CleanFemalebyRegion, key = "Measure", value = "Rate of Income", -Year, -Region) 

#Cleaning the Male income data
male_income %>%
  select(-Number.of.People..000., -Median.Weekly.Earnings,-Median.Hourly.Earnings) -> CleanMaleIncome
CleanMaleIncome <- gather(CleanMaleIncome, key = "Measure Rate", value = "Rate of Income", -Year)

#Cleaning the Male Regional incomes data
male_regional %>%
  select(-Median.hourly.earnings, -Medain.Weekly.earnings, -Number.of.people) -> CleanMalebyRegion
CleanMalebyRegion <- gather(CleanMalebyRegion, key = "Income Measure", value = "Rate of Income", -Region, -Year)

#Clean the Overall Regional income data of New Zealand
regional_income %>%
  select(-Number.of.people, -Median.hourly.earnings, -Medain.Weekly.earnings) -> CleanRegionalIncome
CleanRegionalIncome <- gather(CleanRegionalIncome, key = "Income Measure", value = "Rate of Income" , -Year, -Region)

#All data should be cleaned and ready to be processed into ggplot


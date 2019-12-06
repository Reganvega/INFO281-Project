#Loading up libraries for R
library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)
library(Rmisc)

#Setting work area
setwd("C:/Users/regan/OneDrive/University Materials/2019/Summer/INFO281/Final Project/281 Project/Data")

#Reading data for analysis
Ethnic_Data <- read.csv("EthnicData.csv")
female_income<-read.csv("FemaleIncome.csv")
female_Regional<-read.csv("FemaleRegionalData.csv")
Total_Gender_Income<-read.csv("GenderIncomesTotal.csv")
male_income<-read.csv("MaleIncome.csv")
male_regional<-read.csv("MaleRegionalData.csv")
regional_income<-read.csv("RegionalIncome.csv")

#gg plot
Ethnic_Data <- Rmisc::summarySE

##Scavenging Efficiency Models
#Updated 23 March 2023

#Script for the level one analysis (Scav efficiency) between areas with/without meso mammals detected. Specifically, it is lmms for latency to first scavenging event and time to complete consumption of carcass. 

# #clear environment
remove(list=ls())

# load packages
library(tidyverse)
library(lme4)
library(car)
library(ggplot2)
library(dplyr)


##Read in scav data with times to arrival and time to complete consumption
scav.data<-read.csv("scav.eff.csv")%>%
  mutate(Site=as.factor(Site))
#Values are capped at the end of the experiment 
#mesoPres column indicates whether >=1 mesomammal (possum, raccoon, skunks, rabbits, foxes,etc ) were detected  either in scat/camera surveys of site conducted in early 2019

table(scav.data$mesoPres)
#9 sites, 3 stations per site (27)
#6 sites, 3 stations per site (18)

######
#Time to first arrival 
######

#Linerar mixed model with the time to first arrival (capped at end of 7 day experiment as response variable)
m.T2F<-lme4::lmer(scav.data, formula=(T2F.capped)~1+mesoPres+(1|Site), REML=T)
summary(m.T2F)
car::Anova(m.T2F)

#Checking residuals 
hist(resid(m.T2F))
shapiro.test(resid(m.T2F)) 

sum_mod<-summary(m.T2F)
sum_mod$coefficients

#Time to first scavenging event summaries
sum_T2F<-scav.data%>%
  group_by(mesoPres)%>%
  summarize(mean=mean(T2F.capped), sd=sd(T2F.capped))%>%mutate(lower=(mean-sd), upper=(mean+sd))


######
#Time to total consumption 
######
sum_removal<-scav.data%>%
  group_by(mesoPres)%>%
  summarize(mean=mean(elapsed.capped), sd=sd(elapsed.capped))%>%mutate(lower=(mean-sd), upper=(mean+sd))


##Model for consumption time 
m.consumption<-lme4::lmer(scav.data, formula=(elapsed.capped)~1+mesoPres+(1|Site), REML=T)
summary(m.consumption)
car::Anova(m.consumption)

#Checking residuals 
hist(resid(m.consumption))
shapiro.test(resid(m.consumption))

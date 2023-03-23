##Frugivory Efficiency Models
#Updated 23 Mar 2023

#Script for the level one analysis (frug efficiency) between areas with/without meso mammals detected. Specifically, it is glmms for latency to first frug event and proportion of fruit consumed

#Frugivory 

# #clear environment
remove(list=ls())

# load packages
library(tidyverse)
library(lme4)
library(car)
library(ggplot2)
library(cowplot)
library(dplyr)
library(emmeans)


#load frugivory data
frug_dat<-read.csv("frug.eff.csv")


##Time to first arrival: need to limit to just one fruit as they are duplicate values 
single.fruit<-frug_dat%>%filter(Fruit=="Beautyberry")


##Transformed data

#lme4 and Cars on transformed data
t2f2<-lme4::lmer(data=single.fruit,  formula= sqrt(T2F.capped)~1+mesoPres+ (1|Site))
t2f2
Anova(t2f2)
shapiro.test(resid(t2f2))
hist(resid(t2f2))


#Some Summary Stats
frug_sum_t2f<-single.fruit%>%
  group_by(mesoPres)%>%
  summarize(mean=mean(T2F.capped), sd=sd(T2F.capped), median=median(T2F.capped))%>%mutate(lower=(mean-sd), upper=(mean+sd))


#Proportion of Fruit Remaining with fruit as additive effect,site as random effect

#with lme4: 
consumed.model<-lme4::lmer(data=frug_dat,  formula= (prop.consumed)~1+mesoPres+ Fruit+ (1|Site))
Anova(consumed.model)
emmeans(consumed.model, list(pairwise ~ Fruit), adjust = "tukey")
shapiro.test(resid(consumed.model))

##Examine Interaction 
interact.model<-lme4::lmer(data=frug_dat,  formula= (prop.consumed)~1+mesoPres* Fruit+ (1|Site))
summary(interact.model)
Anova(interact.model)
shapiro.test(resid(interact.model))
hist(resid(interact.model))

frug_sum_con<-frug_dat%>%
  group_by(mesoPres, Fruit)%>%
  summarize(mean=mean(prop.consumed),sd=sd(prop.consumed),count=n(), se=(sd/sqrt(count)))%>%mutate(lower=(mean-sd), upper=(mean+sd))



##Comparison of frugivore/scavenger community composition in Everglades
##Last updated 23 Mar 2023


# #clear environment
remove(list=ls())

# load packages
library(tidyverse)
library (dplyr)
library(camtrapR)
library(reshape2)
library(vegan)



#Input scavenging data
dat_scavenge<-read.csv("scav.com.matrix.csv")

##Format matrix 
dat_scavenge_temp<-dat_scavenge%>%dplyr::select(Reptile:Bear)
scav_matrix<-as.matrix(dat_scavenge_temp)
scav_matrix[is.na(scav_matrix)]<-0


##Calculate distance using log transformation recommended by Anderson
scav_dist.log<-vegdist(decostand(scav_matrix, method="log"), method="jaccard")

#Permanova to assess community difference 
scav.ruz.log<-adonis(scav_dist.log~mesoPres,data = dat_scavenge,permutations = 99999)
scav.ruz.log


#Input frug data
dat_frugivory<-read.csv("frug.com.matrix.csv")

#Format matrix
dat_frugivory_temp<-dat_frugivory%>%dplyr::select(Insect:Reptile)
frugivory_matrix<-as.matrix(dat_frugivory_temp)
frugivory_matrix[is.na(frugivory_matrix)]<-0

##Frugivory

#Calculate distance on abundance with log transformation 
frug_dist.log<-vegdist(decostand(frugivory_matrix,"log"),method='jaccard')

#permanova Log.Abundance
frug.ruz.log<-adonis(frug_dist.log~mesoPres,data = dat_frugivory, permutations = 99999)
frug.ruz.log


##Chi square comparisons of rodent vectors and potential disperers

#Scavenging
##Total of rodents vs. not rodent scavenging bouts 
scav.table<-read.csv("scav.chi.table.csv", header=T,row=1)
scav.table
chisq.test(scav.table)


##Frugivory
#Totals of dispersers/non-dispersers 
frug.table<-read.csv("frug.chi.table.csv",header=T,row=1)
frug.table
chisq.test(frug.table)


# Set the data 
library(MRMCaov)
library(data.table)
library(ggplot2)
library(pROC)
library(flextable)
library(magrittr)
library(boot)
library(officer)

## Read in the Optellum data 
source("src/functions.R")

rr <- fread("data/reader_responses_updated.csv")
ps <- fread("data/partsolid_updated_dataset_jason(11-23-21).csv")
pt <- fread("data/patient_info_updated(4-23-21).csv")

# This bit recodes the scores so that they can "stacked"
s1 = names(rr)[-6]
s2 = names(rr)[-3]
rrlm1 = rr[,..s1]
setnames(rrlm1,old = "LOM1",new = "rating")
rrlm1[,test:="LOM1"]

rrlm2 = rr[,..s2]
setnames(rrlm2,old = "LOM2",new = "rating")
rrlm2[,test:="LOM2"]

rrb = rbind(rrlm1,rrlm2)
setkey(rrb,reader)

## Now merge with the patient file - merging on index. 
DT = merge(rrb,pt,by = "index")

# 55 cases in previous analysis 
DT[,isMixed[1], by = index][,table(V1)] 

# as per email from Roger Kim Tue 23/11/2021
DT[,partSolid := ps$partsolid[match(DT$index,ps$index)]]

# 31 moved from solid to part-solid
DT[,.(isMixed[1],partSolid[1]), by = index][,table(V1,V2)] 


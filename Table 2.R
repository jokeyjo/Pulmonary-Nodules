# Table 2 Sub-group analysis 

source("src/set the data.R")

# Reader Specialty  -------------------------------------------------------------

speciality = c(rep("Pulmonologist",2),rep("Radiologist",2),rep("Pulmonologist",4),rep("Radiologist",4))

DT[,Speciality:= speciality[DT$reader]]

DTs = DT[Speciality == "Pulmonologist"]

a5a <- mrmc(empirical_auc(isCancer, rating),
            test = test,
            reader = reader,
            case = index,
            data = DTs, 
            method = "DeLong",  
            design = 1)


su = summary(a5a)
tms = su$test_means
tdsa = su$test_diffs
m0a <- printfn(tms[1,'Estimate'],tms[1,'CI'][1],tms[1,'CI'][2],scale = 100)
m1a <- printfn(tms[2,'Estimate'],tms[2,'CI'][1],tms[2,'CI'][2],scale = 100)
d1a <- printfn(tdsa$Estimate,tdsa$CI[,'Upper'],tdsa$CI[,'Lower'],scale = -100)

DTs = DT[Speciality == "Radiologist"]
a5b <- mrmc(empirical_auc(isCancer, rating),
            test = test,
            reader = reader,
            case = index,
            data = DTs, 
            method = "DeLong",  
            design = 1)


su = summary(a5b)
tms = su$test_means
tdsb = su$test_diffs

m0b <- printfn(tms[1,'Estimate'],tms[1,'CI'][1],tms[1,'CI'][2],scale = 100)
m1b <- printfn(tms[2,'Estimate'],tms[2,'CI'][1],tms[2,'CI'][2],scale = 100)
d1b <- printfn(tdsb$Estimate,tdsb$CI[,'Upper'],tdsb$CI[,'Lower'],scale = -100)

### comparison 
diff = (tdsb$Estimate - tdsa$Estimate)*100
std.err = sqrt(tdsa$StdErr^2 + tdsb$StdErr^2)
dfs = tdsb$df + tdsa$df
tstat = abs((diff/100)/std.err)
pv = (1 - pt(tstat,dfs))*2

ft1 = data.frame(Variable = c("Pulmonology","Radiology"), 
                 m0 = c(m0a,m0b),
                 m1 = c(m1a,m1b), 
                 delta = c(d1a,d1b),
                 diff = c(diff,diff),
                 p = c(pv,pv))


# Nodule diameter -------------------------------------------------------------

DT[,nodule_size := ifelse(size >= 10,"10mm or larger","less than 10mm")]
DTs = DT[nodule_size == "10mm or larger"]
a6a <- mrmc(empirical_auc(isCancer, rating),
            test = test,
            reader = reader,
            case = index,
            data = DTs, 
            method = "DeLong",  
            design = 1)

su = summary(a6a)
tms = su$test_means
tdsa = su$test_diffs
m0a <- printfn(tms[1,'Estimate'],tms[1,'CI'][1],tms[1,'CI'][2],scale = 100)
m1a <- printfn(tms[2,'Estimate'],tms[2,'CI'][1],tms[2,'CI'][2],scale = 100)
d1a <- printfn(tdsa$Estimate,tdsa$CI[,'Upper'],tdsa$CI[,'Lower'],scale = -100)


DTs = DT[nodule_size == "less than 10mm"]
a6b <- mrmc(empirical_auc(isCancer, rating),
            test = test,
            reader = reader,
            case = index,
            data = DTs, 
            method = "DeLong",  # unbiased (bootstrap) takes a long time
            design = 1)


# these are the estimates overall AUC's 
su = summary(a6b)
tms = su$test_means
tdsb = su$test_diffs
m0b <- printfn(tms[1,'Estimate'],tms[1,'CI'][1],tms[1,'CI'][2],scale = 100)
m1b <- printfn(tms[2,'Estimate'],tms[2,'CI'][1],tms[2,'CI'][2],scale = 100)
d1b <- printfn(tdsb$Estimate,tdsb$CI[,'Upper'],tdsb$CI[,'Lower'],scale = -100)


diff = (tdsa$Estimate - tdsb$Estimate)*100
std.err = sqrt(tdsa$StdErr^2 + tdsb$StdErr^2)
dfs = tdsb$df + tdsa$df
tstat = abs((diff/100)/std.err)
pv = (1 - pt(tstat,dfs))*2


ft2 = data.frame(Variable = c("5 to <10",">= 10 to <=30"), 
                 m1 = c(m0b,m0a), 
                 m0 = c(m1b,m1a),
                 delta = c(d1b,d1a),
                 diff = c(diff,diff),
                 p = c(pv,pv))


# Nodule density  -----------------------------------------------

DTs = DT[isMixed == 1]
a7a <- mrmc(empirical_auc(isCancer, rating),
            test = test,
            reader = reader,
            case = index,
            data = DTs, 
            method = "DeLong",  
            design = 1)

su = summary(a7a)
tms = su$test_means
tdsa = su$test_diffs
m0a <- printfn(tms[1,'Estimate'],tms[1,'CI'][1],tms[1,'CI'][2],scale = 100)
m1a <- printfn(tms[2,'Estimate'],tms[2,'CI'][1],tms[2,'CI'][2],scale = 100)
d1a <- printfn(tdsa$Estimate,tdsa$CI[,'Upper'],tdsa$CI[,'Lower'],scale = -100)


DTs = DT[isMixed==0]
a7b <- mrmc(empirical_auc(isCancer, rating),
            test = test,
            reader = reader,
            case = index,
            data = DTs, 
            method = "DeLong",  
            design = 1)


# these are the estimates overall AUC's 
su = summary(a7b)
tms = su$test_means
tdsb = su$test_diffs
m0b <- printfn(tms[1,'Estimate'],tms[1,'CI'][1],tms[1,'CI'][2],scale = 100)
m1b <- printfn(tms[2,'Estimate'],tms[2,'CI'][1],tms[2,'CI'][2],scale = 100)
d1b <- printfn(tdsb$Estimate,tdsb$CI[,'Upper'],tdsb$CI[,'Lower'],scale = -100)

diff = (tdsb$Estimate - tdsa$Estimate)*100
std.err = sqrt(tdsa$StdErr^2 + tdsb$StdErr^2)
pv = (1 - pnorm(abs((diff/100)/std.err)))*2

ft3 = data.frame(Variable = c("Solid","Part-solid"), 
                 m0 = c(m0b,m0a),
                 m1 = c(m1b,m1a), 
                 delta = c(d1b,d1a),
                 diff = c(diff,diff),
                 p = c(pv,pv))



# Nodule margins -------------------------------------------------------------

DTs = DT[isspiculated == 1]
a10a <- mrmc(empirical_auc(isCancer, rating),
             test = test,
             reader = reader,
             case = index,
             data = DTs, 
             method = "DeLong",  
             design = 1)


# these are the estimates overall AUC's 
su = summary(a10a)
tms = su$test_means
tdsa = su$test_diffs
m0a <- printfn(tms[1,'Estimate'],tms[1,'CI'][1],tms[1,'CI'][2],scale = 100)
m1a <- printfn(tms[2,'Estimate'],tms[2,'CI'][1],tms[2,'CI'][2],scale = 100)
d1a <- printfn(tdsa$Estimate,tdsa$CI[,'Upper'],tdsa$CI[,'Lower'],scale = -100)


DTs = DT[isspiculated == 0]
a10b <- mrmc(empirical_auc(isCancer, rating),
             test = test,
             reader = reader,
             case = index,
             data = DTs, 
             method = "DeLong",  
             design = 1)

# these are the estimates overall AUC's 
su = summary(a10b)
tms = su$test_means
tdsb = su$test_diffs
m0b <- printfn(tms[1,'Estimate'],tms[1,'CI'][1],tms[1,'CI'][2],scale = 100)
m1b <- printfn(tms[2,'Estimate'],tms[2,'CI'][1],tms[2,'CI'][2],scale = 100)
d1b <- printfn(tdsb$Estimate,tdsb$CI[,'Upper'],tdsb$CI[,'Lower'],scale = -100)


### 
diff = (tdsa$Estimate - tdsb$Estimate)*100
std.err = sqrt(tdsa$StdErr^2 + tdsb$StdErr^2)
dfs = tdsb$df + tdsa$df
tstat = abs((diff/100)/std.err)
pv = (1 - pt(tstat,dfs))*2

ft4 = data.frame(Variable = c("Non-spiculated","Spiculated"), 
                 m0 = c(m0b,m0a),
                 m1 = c(m1b,m1a), 
                 delta = c(d1a,d1b),
                 diff = c(diff,diff),
                 p = c(pv,pv))


# Type of Chest CT  ------------------------------------------------

DTs = DT[isScreening == 0] # Diagnostic 

a10a <- mrmc(empirical_auc(isCancer, rating),
             test = test,
             reader = reader,
             case = index,
             data = DTs, 
             method = "DeLong",  
             design = 1)


# these are the estimates overall AUC's 
su = summary(a10a)
tms = su$test_means
tdsa = su$test_diffs
m0a <- printfn(tms[1,'Estimate'],tms[1,'CI'][1],tms[1,'CI'][2],scale = 100)
m1a <- printfn(tms[2,'Estimate'],tms[2,'CI'][1],tms[2,'CI'][2],scale = 100)
d1a <- printfn(tdsa$Estimate,tdsa$CI[,'Upper'],tdsa$CI[,'Lower'],scale = -100)

DTs = DT[isScreening == 1] # Screening 
a10b <- mrmc(empirical_auc(isCancer, rating),
             test = test,
             reader = reader,
             case = index,
             data = DTs, 
             method = "DeLong",  
             design = 1)

# these are the estimates overall AUC's 
su = summary(a10b)
tms = su$test_means
tdsb = su$test_diffs

m0b <- printfn(tms[1,'Estimate'],tms[1,'CI'][1],tms[1,'CI'][2],scale = 100)
m1b <- printfn(tms[2,'Estimate'],tms[2,'CI'][1],tms[2,'CI'][2],scale = 100)
d1b <- printfn(tdsb$Estimate,tdsb$CI[,'Upper'],tdsb$CI[,'Lower'],scale = -100)

### 
diff = (tdsb$Estimate - tdsa$Estimate)*100
std.err = sqrt(tdsa$StdErr^2 + tdsb$StdErr^2)
dfs = tdsb$df + tdsa$df
tstat = abs((diff/100)/std.err)
pv = (1 - pt(tstat,dfs))*2

ft5 = data.frame(Variable = c("Diagnostic","Screening"), 
                 m0 = c(m0b,m0a),
                 m1 = c(m1b,m1a), 
                 delta = c(d1b,d1a),
                 diff = c(diff,diff),
                 p = c(pv,pv))


# Combine tables  ---------------------------------------------------------

FT <- rbind(ft1,ft2,ft3,ft4,ft5)

FT = flextable(FT)
FT <- FT %>% 
  set_header_labels(`m0` = "Without AI",`m1` = "With AI",`delta` = "With AI - W/O AI",
                    `diff` = "Delta AUC",`p` = "p value") %>%
  align(part = "all",align = "center") %>% 
  merge_at(i = 1:2,j = 5) %>% 
  merge_at(i = 1:2,j = 6) %>% 
  merge_at(i = 3:4,j = 5) %>% 
  merge_at(i = 3:4,j = 6) %>% 
  merge_at(i = 5:6,j = 5) %>% 
  merge_at(i = 5:6,j = 6) %>% 
  merge_at(i = 7:8,j = 5) %>% 
  merge_at(i = 7:8,j = 6) %>% 
  merge_at(i = 9:10,j = 5) %>% 
  merge_at(i = 9:10,j = 6) %>% 
  colformat_double(j = 5, digits = 2) %>%
  colformat_double(j = 6, digits = 3) %>%
  border_inner_h() %>% 
  autofit()

FT


# Send to Officer ---------------------------------------------------------

my_doc <- read_docx()
my_doc <- my_doc %>% 
  body_add_par(paste0("Date: ",date()), style = "Normal") %>% 
  body_add_par("", style = "Normal") %>%  
  body_add_par("Table 2: Mean Reader Discrimination of Indeterminate Pulmonary Nodules with and without
Computer-aided Diagnosis in Prespecified Subgroups", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%  
  body_add_par("", style = "Normal") %>%   
  body_add_flextable(FT) %>% 
  print(my_doc, target = "tables/Table 2.docx")


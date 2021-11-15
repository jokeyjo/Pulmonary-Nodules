# Table 3 

source("src/set the data.R")

# Classification of Pulmonary Nodules with Computer-aided diagnosis 

# Five percent likelihood of malignancy -----------------------------------

DT[,pos5perc := ifelse(rating >= 5,1,0)]
m <-  DT[test == "LOM1", table(isCancer,pos5perc)]
c1 <- c(m[2,2],m[2,1],m[1,1],m[1,2])
m <-  DT[test == "LOM2", table(isCancer,pos5perc)]
c2 <- c(m[2,2],m[2,1],m[1,1],m[1,2])


a5sens <- mrmc(binary_sens(isCancer,pos5perc),
               test = test,
               reader = reader,
               case = index,
               data = DT, 
               method = "jackknife",
               design = 1)

a5fnr <- mrmc(binary_FN(isCancer,pos5perc),
               test = test,
               reader = reader,
               case = index,
               data = DT, 
               method = "jackknife",
               design = 1)

a5spec <- mrmc(binary_spec(isCancer,pos5perc),
               test = test,
               reader = reader,
               case = index,
               data = DT, 
               method = "jackknife",
               design = 1)

a5fps <- mrmc(binary_FP(isCancer,pos5perc),
              test = test,
              reader = reader,
              case = index,
              data = DT, 
              method = "jackknife",
              design = 1)

su.a5sens = summary(a5sens)
su.a5fnr = summary(a5fnr)
su.a5spec = summary(a5spec)
su.a5fps = summary(a5fps)


tms = su.a5sens$test_means
tds = su.a5sens$test_diffs
snsa =  printfn(tms[1,'Estimate'], tms[1,'CI'][1], tms[1,'CI'][2], scale= 100)
snsb = printfn(tms[2,'Estimate'], tms[2,'CI'][1], tms[2,'CI'][2], scale= 100)
dsns = printfn(tds['Estimate']*-1,tds[1,'CI'][1,2]*-1, tds[1,'CI'][1,1]*-1, scale= 100)

tms = su.a5fnr$test_means
tds = su.a5fnr$test_diffs
fna =  printfn(tms[1,'Estimate'], tms[1,'CI'][1], tms[1,'CI'][2], scale= 100)
fnb = printfn(tms[2,'Estimate'], tms[2,'CI'][1], tms[2,'CI'][2], scale= 100)
dfn = printfn(tds['Estimate']*-1,tds[1,'CI'][1,2]*-1, tds[1,'CI'][1,1]*-1, scale= 100)

tms = su.a5spec$test_means
tds = su.a5spec$test_diffs
spa = printfn(tms[1,'Estimate'], tms[1,'CI'][1], tms[1,'CI'][2], scale= 100)
spb = printfn(tms[2,'Estimate'], tms[2,'CI'][1], tms[2,'CI'][2], scale= 100)
dsp = printfn(tds['Estimate']*-1,tds[1,'CI'][1,2]*-1, tds[1,'CI'][1,1]*-1, scale= 100)

tms = su.a5fps$test_means
tds = su.a5fps$test_diffs
fpa = printfn(tms[1,'Estimate'], tms[1,'CI'][1], tms[1,'CI'][2], scale= 100)
fpb = printfn(tms[2,'Estimate'], tms[2,'CI'][1], tms[2,'CI'][2], scale= 100)
dfp = printfn(tds['Estimate']*-1,tds[1,'CI'][1,2]*-1, tds[1,'CI'][1,1]*-1, scale= 100)

spec.pval = su.a5spec$test_equality$`p-value`
sens.pval = su.a5sens$test_equality$`p-value`
fns.pval = su.a5fns$test_equality$`p-value`
fps.pval = su.a5fps$test_equality$`p-value`

sens.ft5a = data.frame(Threshold = "5%", Sns1 = snsa, Sns2 = snsb, Dsns = dsns,Pval = sens.pval)
spec.ft5a = data.frame(Threshold = "5%", Sp1 = spa, Sp2 = spb,Dsp = dsp, Pval = spec.pval)
fps.ft5a = data.frame(Threshold = "5%", Fp1 = fpa, Fp2 = fpb, Dfp = dfp, Pval = fps.pval)
fns.ft5a = data.frame(Threshold = "5%", Fn1 = fna, Fn2 = fnb,Dfn = dfn, Pval = fns.pval)


# Threshold @ 10%  --------------------------------------------------------

## Threshold @ 10% 
DT[,pos10perc := ifelse(rating >= 10,1,0)]

a10sens <- mrmc(binary_sens(isCancer,pos10perc),
               test = test,
               reader = reader,
               case = index,
               data = DT, 
               method = "jackknife",
               design = 1)


a10fnr <- mrmc(binary_FN(isCancer,pos10perc),
              test = test,
              reader = reader,
              case = index,
              data = DT, 
              method = "jackknife",
              design = 1)

a10spec <- mrmc(binary_spec(isCancer,pos10perc),
               test = test,
               reader = reader,
               case = index,
               data = DT, 
               method = "jackknife",
               design = 1)

a10fps <- mrmc(binary_FP(isCancer,pos10perc),
              test = test,
              reader = reader,
              case = index,
              data = DT, 
              method = "jackknife",
              design = 1)

su.a10sens = summary(a10sens)
su.a10fnr = summary(a10fnr)
su.a10spec = summary(a10spec)
su.a10fps = summary(a10fps)


tms = su.a10sens$test_means
tds = su.a10sens$test_diffs
snsa =  printfn(tms[1,'Estimate'], tms[1,'CI'][1], tms[1,'CI'][2], scale= 100)
snsb = printfn(tms[2,'Estimate'], tms[2,'CI'][1], tms[2,'CI'][2], scale= 100)
dsns = printfn(tds['Estimate']*-1,tds[1,'CI'][1,2]*-1, tds[1,'CI'][1,1]*-1, scale= 100)


tms = su.a10fnr$test_means
tds = su.a10fnr$test_diffs
fna =  printfn(tms[1,'Estimate'], tms[1,'CI'][1], tms[1,'CI'][2], scale= 100)
fnb = printfn(tms[2,'Estimate'], tms[2,'CI'][1], tms[2,'CI'][2], scale= 100)
dfn = printfn(tds['Estimate']*-1,tds[1,'CI'][1,2]*-1, tds[1,'CI'][1,1]*-1, scale= 100)

tms = su.a10spec$test_means
tds = su.a10spec$test_diffs
spa = printfn(tms[1,'Estimate'], tms[1,'CI'][1], tms[1,'CI'][2], scale= 100)
spb = printfn(tms[2,'Estimate'], tms[2,'CI'][1], tms[2,'CI'][2], scale= 100)
dsp = printfn(tds['Estimate']*-1,tds[1,'CI'][1,2]*-1, tds[1,'CI'][1,1]*-1, scale= 100)

tms = su.a10fps$test_means
tds = su.a10fps$test_diffs
fpa = printfn(tms[1,'Estimate'], tms[1,'CI'][1], tms[1,'CI'][2], scale= 100)
fpb = printfn(tms[2,'Estimate'], tms[2,'CI'][1], tms[2,'CI'][2], scale= 100)
dfp = printfn(tds['Estimate']*-1,tds[1,'CI'][1,2]*-1, tds[1,'CI'][1,1]*-1, scale= 100)

spec.pval = su.a10spec$test_equality$`p-value`
sens.pval = su.a10sens$test_equality$`p-value`
fns.pval = su.a10fns$test_equality$`p-value`
fps.pval = su.a10fps$test_equality$`p-value`

sens.ft10a = data.frame(Threshold = "10%", Sns1 = snsa, Sns2 = snsb, Dsns = dsns,Pval = sens.pval)
spec.ft10a = data.frame(Threshold = "10%", Sp1 = spa, Sp2 = spb,Dsp = dsp, Pval = spec.pval)
fps.ft10a = data.frame(Threshold = "10%", Fp1 = fpa, Fp2 = fpb, Dfp = dfp, Pval = fps.pval)
fns.ft10a = data.frame(Threshold = "10%", Fn1 = fna, Fn2 = fnb,Dfn = dfn, Pval = fns.pval)



# 65% Threshold -----------------------------------------------------------

DT[,pos65perc := ifelse(rating > 65,1,0)]
m <-  DT[test == "LOM1", table(isCancer,pos65perc)]
c3 <- c(m[2,2],m[2,1],m[1,1],m[1,2])
m <-  DT[test == "LOM2", table(isCancer,pos65perc)]
c4 <- c(m[2,2],m[2,1],m[1,1],m[1,2])

a65sens <- mrmc(binary_sens(isCancer,pos65perc),
                test = test,
                reader = reader,
                case = index,
                data = DT, 
                method = "jackknife",
                design = 1)


a65fnr <- mrmc(binary_FN(isCancer,pos65perc),
               test = test,
               reader = reader,
               case = index,
               data = DT, 
               method = "jackknife",
               design = 1)

a65spec <- mrmc(binary_spec(isCancer,pos65perc),
                test = test,
                reader = reader,
                case = index,
                data = DT, 
                method = "jackknife",
                design = 1)

a65fps <- mrmc(binary_FP(isCancer,pos65perc),
               test = test,
               reader = reader,
               case = index,
               data = DT, 
               method = "jackknife",
               design = 1)

su.a65sens = summary(a65sens)
su.a65fnr = summary(a65fnr)
su.a65spec = summary(a65spec)
su.a65fps = summary(a65fps)


tms = su.a65sens$test_means
tds = su.a65sens$test_diffs
snsa =  printfn(tms[1,'Estimate'], tms[1,'CI'][1], tms[1,'CI'][2], scale= 100)
snsb = printfn(tms[2,'Estimate'], tms[2,'CI'][1], tms[2,'CI'][2], scale= 100)
dsns = printfn(tds['Estimate']*-1,tds[1,'CI'][1,2]*-1, tds[1,'CI'][1,1]*-1, scale= 100)


tms = su.a65fnr$test_means
tds = su.a65fnr$test_diffs
fna =  printfn(tms[1,'Estimate'], tms[1,'CI'][1], tms[1,'CI'][2], scale= 100)
fnb = printfn(tms[2,'Estimate'], tms[2,'CI'][1], tms[2,'CI'][2], scale= 100)
dfn = printfn(tds['Estimate']*-1,tds[1,'CI'][1,2]*-1, tds[1,'CI'][1,1]*-1, scale= 100)

tms = su.a65spec$test_means
tds = su.a65spec$test_diffs
spa = printfn(tms[1,'Estimate'], tms[1,'CI'][1], tms[1,'CI'][2], scale= 100)
spb = printfn(tms[2,'Estimate'], tms[2,'CI'][1], tms[2,'CI'][2], scale= 100)
dsp = printfn(tds['Estimate']*-1,tds[1,'CI'][1,2]*-1, tds[1,'CI'][1,1]*-1, scale= 100)

tms = su.a65fps$test_means
tds = su.a65fps$test_diffs
fpa = printfn(tms[1,'Estimate'], tms[1,'CI'][1], tms[1,'CI'][2], scale= 100)
fpb = printfn(tms[2,'Estimate'], tms[2,'CI'][1], tms[2,'CI'][2], scale= 100)
dfp = printfn(tds['Estimate']*-1,tds[1,'CI'][1,2]*-1, tds[1,'CI'][1,1]*-1, scale= 100)

spec.pval = su.a65spec$test_equality$`p-value`
sens.pval = su.a65sens$test_equality$`p-value`
fns.pval = su.a65fns$test_equality$`p-value`
fps.pval = su.a65fps$test_equality$`p-value`

sens.ft65a = data.frame(Threshold = "65%", Sns1 = snsa, Sns2 = snsb, Dsns = dsns,Pval = sens.pval)
spec.ft65a = data.frame(Threshold = "65%", Sp1 = spa, Sp2 = spb,Dsp = dsp, Pval = spec.pval)
fps.ft65a = data.frame(Threshold = "65%", Fp1 = fpa, Fp2 = fpb, Dfp = dfp, Pval = fps.pval)
fns.ft65a = data.frame(Threshold = "65%", Fn1 = fna, Fn2 = fnb,Dfn = dfn, Pval = fns.pval)


# Seventy percent likelihood of malignancy --------------------------------


DT[,pos70perc := ifelse(rating >= 70,1,0)]

a70sens <- mrmc(binary_sens(isCancer,pos70perc),
                test = test,
                reader = reader,
                case = index,
                data = DT, 
                method = "jackknife",
                design = 1)


a70fnr <- mrmc(binary_FN(isCancer,pos70perc),
               test = test,
               reader = reader,
               case = index,
               data = DT, 
               method = "jackknife",
               design = 1)

a70spec <- mrmc(binary_spec(isCancer,pos70perc),
                test = test,
                reader = reader,
                case = index,
                data = DT, 
                method = "jackknife",
                design = 1)

a70fps <- mrmc(binary_FP(isCancer,pos70perc),
               test = test,
               reader = reader,
               case = index,
               data = DT, 
               method = "jackknife",
               design = 1)

su.a70sens = summary(a70sens)
su.a70fnr = summary(a70fnr)
su.a70spec = summary(a70spec)
su.a70fps = summary(a70fps)


tms = su.a70sens$test_means
tds = su.a70sens$test_diffs
snsa =  printfn(tms[1,'Estimate'], tms[1,'CI'][1], tms[1,'CI'][2], scale= 100)
snsb = printfn(tms[2,'Estimate'], tms[2,'CI'][1], tms[2,'CI'][2], scale= 100)
dsns = printfn(tds['Estimate']*-1,tds[1,'CI'][1,2]*-1, tds[1,'CI'][1,1]*-1, scale= 100)


tms = su.a70fnr$test_means
tds = su.a70fnr$test_diffs
fna =  printfn(tms[1,'Estimate'], tms[1,'CI'][1], tms[1,'CI'][2], scale= 100)
fnb = printfn(tms[2,'Estimate'], tms[2,'CI'][1], tms[2,'CI'][2], scale= 100)
dfn = printfn(tds['Estimate']*-1,tds[1,'CI'][1,2]*-1, tds[1,'CI'][1,1]*-1, scale= 100)

tms = su.a70spec$test_means
tds = su.a70spec$test_diffs
spa = printfn(tms[1,'Estimate'], tms[1,'CI'][1], tms[1,'CI'][2], scale= 100)
spb = printfn(tms[2,'Estimate'], tms[2,'CI'][1], tms[2,'CI'][2], scale= 100)
dsp = printfn(tds['Estimate']*-1,tds[1,'CI'][1,2]*-1, tds[1,'CI'][1,1]*-1, scale= 100)

tms = su.a70fps$test_means
tds = su.a70fps$test_diffs
fpa = printfn(tms[1,'Estimate'], tms[1,'CI'][1], tms[1,'CI'][2])
fpb = printfn(tms[2,'Estimate'], tms[2,'CI'][1], tms[2,'CI'][2])
dfp = printfn(tds['Estimate']*-1,tds[1,'CI'][1,2]*-1, tds[1,'CI'][1,1]*-1)

spec.pval = su.a70spec$test_equality$`p-value`
sens.pval = su.a70sens$test_equality$`p-value`
fns.pval = su.a70fns$test_equality$`p-value`
fps.pval = su.a70fps$test_equality$`p-value`

sens.ft70a = data.frame(Threshold = "70%", Sns1 = snsa, Sns2 = snsb, Dsns = dsns,Pval = sens.pval)
spec.ft70a = data.frame(Threshold = "70%", Sp1 = spa, Sp2 = spb,Dsp = dsp, Pval = spec.pval)
fps.ft70a = data.frame(Threshold = "70%", Fp1 = fpa, Fp2 = fpb, Dfp = dfp, Pval = fps.pval)
fns.ft70a = data.frame(Threshold = "70%", Fn1 = fna, Fn2 = fnb,Dfn = dfn, Pval = fns.pval)


# Top half of table  ------------------------------------------------------
classifier = c("True-positive results, n",
               "False-negative results, n",
               "True-negative results, n",
               "False-positive results, n")
dt <- data.frame(classifier,c1,c2,c3,c4)

ft0 = flextable(dt)
ft0 <- ft0 %>% 
  add_header(classifier = "",c1 = "5%",c2 = "5%", c3 = "65%", c4 = "65%") %>% 
  set_header_labels(classifier = "Classification performance",
             c1 = "Without CAD", 
             c2 = "With CAD", 
             c3 = "Without CAD", 
             c4 = "With CAD") %>% 
  merge_h(part = "header") %>% 
  align(part = "all",align = "center") %>% 
  #colformat_double(j = 2,digits = 3) %>% 
  autofit()
ft0

       
# Sensitivity --------------------------------------------------------------
ft2 = rbind(sens.ft5a,sens.ft10a,sens.ft65a,sens.ft70a)

# merge horizontally when there are identical values
ft2 = flextable(ft2)
ft2 <- ft2 %>% 
  add_header(Sns1 = "Sensitivity",Sns2 = "Sensitivity",Dsns = "Sensitivity",Pval = "Sensitivity") %>% 
  merge_h(part = "header") %>% 
  set_header_labels(`Sns1` = "(-) CAD",`Sns2` = "(+) CAD",
                    `Dsns` = "Delta",`Pval` = "P value") %>% 
  align(part = "all",align = "center") %>% 
  colformat_double(j = 5,digits = 3) %>% 
  autofit()
ft2




# Specificity ---------------------------------------------------------------

ft3 = rbind(spec.ft5a,spec.ft10a,spec.ft65a,spec.ft70a)

# merge horizontally when there are identical values
ft3 = flextable(ft3)
ft3 <- ft3 %>% 
  add_header(Sp1 = "Specificity",Sp2 = "Specificity",Dsp = "Specificity",Pval = "Specificity") %>% 
  merge_h(part = "header") %>% 
  set_header_labels(`Sp1` = "(-) CAD",`Sp2` = "(+) CAD",`Dsp` = "Delta",`Pval` = "P value") %>% 
  align(part = "all",align = "center") %>% 
  colformat_double(j = 5,digits = 3) %>% 
  autofit()                  

ft3

# False positives ---------------------------------------------------------------

ft4 = rbind(fps.ft5a,fps.ft10a,fps.ft65a,fps.ft70a)

# merge horizontally when there are identical values
ft4 = flextable(ft4)
ft4 <- ft4 %>% 
  add_header(Fp1 = "False positive rate",Fp2 = "False positive rate",Dfp = "False positive rate",Pval = "False positive rate") %>% 
  merge_h(part = "header") %>% 
  set_header_labels(`Fp1` = "(-) CAD",`Fp2` = "(+) CAD",`Dfp` = "Delta",`Pval` = "P value") %>% 
  align(part = "all",align = "center") %>% 
  colformat_double(j = 5,digits = 3) %>% 
  autofit()                  

ft4


# False negative rate ---------------------------------------------------------------

ft5 = rbind(fns.ft5a,fns.ft10a,fns.ft65a,fns.ft70a)

# merge horizontally when there are identical values
ft5 = flextable(ft5)
ft5 <- ft5 %>% 
  add_header(Fn1 = "False negative rate",Fn2 = "False negative rate",Dfn = "False negative rate",Pval = "False negative rate") %>% 
  merge_h(part = "header") %>% 
  set_header_labels(`Fn1` = "(-) CAD",`Fn2` = "(+) CAD",`Dfn` = "Delta",`Pval` = "P value") %>% 
  align(part = "all",align = "center") %>% 
  colformat_double(j = 5,digits = 3) %>% 
  autofit()                  

ft5

# Send to word  -----------------------------------------------------------
my_doc <- read_docx()
my_doc <- my_doc %>% 
  body_add_par(paste0("Date: ",date()), style = "Normal") %>% 
  body_add_par("", style = "Normal") %>%  
  body_add_par("Table 3: Classification of Pulmonary Nodules with Computer-aided Diagnosis", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%  
  body_add_par("", style = "Normal") %>%   
  body_add_flextable(ft0) %>% 
  body_add_par("", style = "Normal") %>%  
  body_add_par("", style = "Normal") %>%   
  body_add_flextable(ft2) %>% 
  body_add_par("", style = "Normal") %>%   
  body_add_par("", style = "Normal") %>%   
  body_add_flextable(ft3) %>% 
  body_add_par("", style = "Normal") %>%   
  body_add_par("", style = "Normal") %>%   
  body_add_flextable(ft4) %>% 
  body_add_par("", style = "Normal") %>%   
  body_add_par("", style = "Normal") %>%   
  body_add_flextable(ft5) %>% 
  print(my_doc, target = "tables/Table 3 v2.docx")



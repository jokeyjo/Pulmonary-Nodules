# Table 4 Net reclassification index 

source("src/set the data.R")

LOM1r = DT[test=="LOM1",rating]
LOM2r = DT[test=="LOM2", rating]
index = DT[test=="LOM1",index]
reader = DT[test=="LOM1",reader]
isCancer = DT[test=="LOM1",isCancer]
wdt = data.table(LOM1r,LOM2r,index,reader,isCancer)

## At five percent 
cutoff = c(0,5,100)/100
(bt5 = boot(wdt,bf,R = 1e04,strata = wdt$isCancer))
org = bt5$t0

bci1 = boot.ci(bt5, type = "perc",index = 1)
bci2 = boot.ci(bt5, type = "perc",index = 2)
bci3 = boot.ci(bt5, type = "perc",index = 3)
bci4 = boot.ci(bt5, type = "perc",index = 4)
bci5 = boot.ci(bt5, type = "perc",index = 5)
bci6 = boot.ci(bt5, type = "perc",index = 6)
bci7 = boot.ci(bt5, type = "perc",index = 7)


# proportion of cancer cases reclassified 
cup5pc <- printfn(org[1],bci1$percent[4],bci1$percent[5],scale=100)
cdwn5pc <- printfn(org[2],bci2$percent[4],bci2$percent[5],scale=100)
net5pc <- printfn(org[3],bci3$percent[4],bci3$percent[5],scale=100)

# Proportion of benign cases reclassified 
bdwn5pc <- printfn(org[4],bci4$percent[4],bci4$percent[5],scale=100)
bup5pc <- printfn(org[5],bci5$percent[4],bci5$percent[5],scale=100)
netbgn5pc <- printfn(org[6],bci6$percent[4],bci6$percent[5],scale=100)
overall5pc <- printfn(org[7],bci7$percent[4],bci7$percent[5],scale=100)

## At <= 65%  
cutoff = c(0,66,100)/100
bt65 = boot(wdt,bf,R = 1e04,strata = wdt$isCancer)
org = bt65$t0

bci1 = boot.ci(bt65, type = "perc",index = 1)
bci2 = boot.ci(bt65, type = "perc",index = 2)
bci3 = boot.ci(bt65, type = "perc",index = 3)
bci4 = boot.ci(bt65, type = "perc",index = 4)
bci5 = boot.ci(bt65, type = "perc",index = 5)
bci6 = boot.ci(bt65, type = "perc",index = 6)
bci7 = boot.ci(bt65, type = "perc",index = 7)

# proportion of cancer cases reclassified 
cup65pc <- printfn(org[1],bci1$percent[4],bci1$percent[5],scale=100)
cdwn65pc <- printfn(org[2],bci2$percent[4],bci2$percent[5],scale=100)
net65pc <- printfn(org[3],bci3$percent[4],bci3$percent[5],scale=100)

# Proportion of benign cases reclassified 
bdwn65pc <- printfn(org[4],bci4$percent[4],bci4$percent[5],scale=100)
bup65pc <- printfn(org[5],bci5$percent[4],bci5$percent[5],scale=100)
netbgn65pc <- printfn(org[6],bci6$percent[4],bci6$percent[5],scale=100)
overall65pc <- printfn(org[7],bci7$percent[4],bci7$percent[5],scale=100)


# Create the flextable  ---------------------------------------------------
Proportion <- factor(c(rep("Cancer cases reclassified, % (95% CI)",3),
               rep("Benign cases reclassified, % (95% CI)",3),"Overall"))

c1 <- c("Cancer up","Cancer down","Net cancer","Benign down","Benign up","Net benign","Overall")
c2 <- c(cup5pc,cdwn5pc,net5pc,bdwn5pc,bup5pc,netbgn5pc,overall5pc)
c3 <- c(cup65pc,cdwn65pc,net65pc,bdwn65pc,bup65pc,netbgn65pc,overall65pc)
dt = data.frame(Proportion,c1,c2,c3)

# merge horizontally when there are identical values
fdt <- as_grouped_data(dt,groups = "Proportion") %>% 
  as_flextable() %>% 
  set_header_labels(c1 = "",
                    c2 = "5%", 
                    c3 = "65%") %>% 
  bold(i = ~ !is.na(Proportion), bold = TRUE) %>% 
  italic(i = ~ !is.na(Proportion), italic = TRUE) %>% 
  fontsize(part = "all",size = 10) %>%
  theme_box()  %>% 
  align(j = 2:3, align = "center", part = "all") %>% 
  autofit() 
fdt


save_as_docx(fdt, path = "Tables/Table 4.docx")



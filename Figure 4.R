# Figure 4   

source("src/set the data.R")

a2 <- mrmc(empirical_auc(isCancer, rating),
           test = test,
           reader = fixed(reader),
           case = index,
           data = DT, 
           method = "DeLong",
           design = 1)
su2 = summary(a2)
rds = su2$reader_test_diffs
rm = su2$reader_means

rds.out = rds[,c(1,2,3,4,5,7)]

# Format the reader level AUC's and confidence intervals
c1 <- printfn(rm$empirical_auc[rm$test=="LOM1"],
              lci = rm$CI[rm$test=="LOM1",'Lower'],
              uci = rm$CI[rm$test=="LOM1",'Upper'],
              scale = 100)
    
c2 <- printfn(est = rm$empirical_auc[rm$test=="LOM2"],
             lci = rm$CI[rm$test=="LOM2",'Lower'],
             uci = rm$CI[rm$test=="LOM2",'Upper'],scale=100)

c3 <- printfn(est = rds$Estimate,
              lci = rds$CI[,'Upper'],
              uci = rds$CI[,'Lower'], 
              scale = -100)

pv = rds$`p-value`
pdt = data.frame(Reader = 1:12, AUC_LM1 = c1, AUC_LM2 = c2,delta = c3,pv = pv)

ft <- flextable(pdt)
ft <- ft %>% 
    set_header_labels(AUC_LM1 = "AUC (95% CI) W/out CADx",
                      AUC_LM2 = "AUC (95% CI) with CADx", 
                      delta = "Delta AUC (95% CI)",
                      pv = "P value") %>% 
    fontsize(part = "all",size = 10) %>%
    theme_box()  %>% 
    align(j = 2:3, align = "center", part = "all") %>% 
    colformat_double(j = 5, digits = 3) %>% 
    autofit() 
ft


save_as_docx(ft, path = "Tables/Figure 4 table.docx")


# Reader level ROC plot (not used)  --------------------------------------------------

# Global plot options 
par(mfrow = c(3,4),
    cex = 0.7,
    mar = c(0.4, 0.4, 0.4, 0.4), 
    oma = c(4, 4, 2, 2), 
    tcl = -0.25, 
    mgp = c(2, 0.6, 0), bg = "white")

# Loop over the 12 readers 
for (i in 1:12){
    t1 = DT[reader==i & test == "LOM1"]
    t2 = DT[reader==i & test == "LOM2"]
    roc1 = roc(t1$isCancer,t1$rating)
    roc2 = roc(t2$isCancer,t2$rating)
    tpr1 = roc1$sensitivities
    tpr2 = roc2$sensitivities
    fpr1 = 1 - roc1$specificities
    fpr2 = 1 - roc2$specificities
    
    plot.new()
    
    plot.window(xlim = c(0,1),ylim =c(0,1))
    
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray96")
    
    box(col = "grey60")
    
    grid(col = "white")
    
    legend(x = 0.5, y = 0.3, 
           legend=c(paste0("W/O AI: ",c1[i],"%"), 
                    paste0("With AI: ", c2[i],"%")),
           col=c("steelblue3","gold2"), lty = c(1,1), cex=0.55,
           title="AUC (95% CI)", text.font=4, bg='azure')
    
    lines(x = fpr1, y= tpr1,lwd =1.4, type = "l",col = "steelblue3",xaxt = "n",yaxt = "n")
    
    lines(x = fpr2, y = tpr2,lwd = 1.4, type = "l", col = "gold2")
    
    if (i %in% c(1, 5, 9)){
        axis(2, col = "grey40", col.axis = "grey20")}
    if (i %in% 9:12){
        axis(1, col = "grey40", col.axis = "grey20")}
    
    mtext(paste0("Reader: ",i), side = 3, line = -1, adj = 0.05, cex = 0.6,col = "black")
}

mtext("FPR (1 - specificity)", side = 1, outer = TRUE, cex = 0.7, line = 2.2,col = "grey20")

mtext("Sensitivity (TPR)", side = 2, outer = TRUE, cex = 0.7, line = 2.2,col = "grey20")



